use crate::common::Opt;
use crate::framework::{DatabaseConfig, IntegrationTest, TestConfig};

use crate::rust_tests::test_bad_filter::*;
use crate::rust_tests::test_find_by::*;

inventory::collect!(IntegrationTest);

#[tokio::main]
pub(crate) async fn run_tests(op: &Opt) -> bool {
    let config = TestConfig {
        db_config: DatabaseConfig::Sqlite,
        optimize: true,
    };

    test_bad_filter(config.clone()).await;
    test_find_by(config.clone()).await;
    for test in inventory::iter::<IntegrationTest> {
        println!("{}", test.name);
        test.test_fn.call(config.clone()).await;
    }

    return true;
}
