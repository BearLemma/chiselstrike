use crate::common::Opt;
use crate::framework::{DatabaseConfig, IntegrationTest, TestConfig};

inventory::collect!(IntegrationTest);

#[tokio::main]
pub(crate) async fn run_tests(op: &Opt) -> bool {
    let config = TestConfig {
        db_config: DatabaseConfig::Sqlite,
        optimize: true,
    };

    for test in inventory::iter::<IntegrationTest> {
        println!("{}", test.name);
        test.test_fn.call(config.clone()).await;
    }

    return true;
}
