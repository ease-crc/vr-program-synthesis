#include <Eigen/Geometry>
#include <vector>
#include <string>

#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>


class Datapoint {
public:
    Datapoint(double timestamp, const std::string& referenceFrame, const Eigen::Vector3d& trans, const Eigen::Quaterniond& rot)
        : timestamp_(timestamp), referenceFrame_(referenceFrame), trans_(trans),
          rot_(rot) {};
    static Datapoint fromProlog(const std::string& datapointString);
public:
    double timestamp_;
    std::string referenceFrame_;
    Eigen::Vector3d trans_;
    Eigen::Quaterniond rot_;
};

class Trajectory
{
public:
    Trajectory(const std::vector<Datapoint>& datapoints)
        : datapoints_(datapoints) {};

    static Trajectory fromProlog(const std::string&  trajString);

    std::vector<Datapoint> datapoints_;
};

const double TRANS_LINEAR_EPS = 1e-6;
const double ROT_LINEAR_EPS = 1e-6;

bool motion_is_linear_trans(const Trajectory& traj);
bool motion_is_linear_rot(const Trajectory& traj);