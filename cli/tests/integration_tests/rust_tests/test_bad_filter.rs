use crate::framework::{IntegrationTest, OpMode, TestConfig};

inventory::submit!(IntegrationTest {
    name: "test_bad_filter",
    test_fn: &test_bad_filter
});

#[log_entry_and_exit(hello, "world")]
pub async fn bagr(config: TestConfig) {}

pub async fn test_bad_filter(config: TestConfig) {
    dummy();
    let mut ctx = config.setup_env(OpMode::Deno).await;
    let (chisel, _chiseld) = ctx.get_chisels();

    chisel.copy("examples/person.ts", "models");
    chisel.write(
        "endpoints/query.ts",
        r##"
        import { Person } from "../models/person.ts";

        export default async function chisel(req: Request) {
            let ret = "";
            const filtered = await Person.findMany({"foo": "bar"});
            filtered.forEach(row => {
                ret += row.first_name + " " + row.last_name + "\n";
            });
            return new Response(ret);
        }
    "##,
    );

    let err = chisel.apply().expect_err("chisel apply should have failed");
    err.stderr()
        .must_contain_ord("endpoints/query.ts:6:53 - error TS2769: No overload matches this call.")
        .must_contain_ord("Argument of type '{ foo: string; }' is not assignable to parameter of type 'Partial<Person>'");
}
