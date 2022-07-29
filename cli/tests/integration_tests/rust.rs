use crate::common::Opt;
use crate::framework::{DatabaseConfig, TestConfig};

use crate::native::test_bad_filter::*;
use crate::native::test_find_by::*;

#[tokio::main]
pub(crate) async fn run_tests(op: &Opt) -> bool {
    let config = TestConfig {
        db_config: DatabaseConfig::Sqlite,
        optimize: true,
    };

    test_bad_filter(config.clone()).await;
    test_find_by(config.clone()).await;
    return true;
}
