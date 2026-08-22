% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention: Live Technical Core, Ceremonial Societal Memory
 *   domain: governance/disaster-preparedness/institutional-memory
 *
 * SUMMARY:
 *   A delta nation's preparedness regime runs on two tracks. Track one is
 *   live: Rijkswaterstaat and the water boards design, inspect, and operate
 *   dikes, storm-surge barriers, and river systems with continuously
 *   exercised, world-class technical competence, funded by levies and
 *   statute. Track two is ceremonial: broader societal memory of flood risk —
 *   resident awareness, evacuation literacy, municipal flood drilling —
 *   persists mainly as commemoration of the 1953 North Sea Flood, awareness
 *   campaigns, and scripted annual exercises that rehearse documents rather
 *   than decisions. Each generation of residents lives further from lived
 *   flood experience while the assurance narrative ('the experts have this')
 *   converts self-protection capacity into audiencehood. The arrangement must
 *   be actively maintained: statutes concentrate technical authority, funding
 *   formulas starve peripheral operational depth, and the commemorative
 *   calendar reproduces the feeling of retention without its substance. This
 *   file instantiates the hybrid_reading of the preparedness_retention
 *   kernel; the sibling readings are separate constraints linked in the
 *   network section. KEY AGENTS (by structural relationship): -
 *   specialized_water_agencies: agenda_setter and primary beneficiary
 *   (institutional/arbitrage) — administers the stratification and collects
 *   levy revenue, mandate expansion, and definitional authority -
 *   water_engineering_professionals: beneficiary (organized/identity_locked)
 *   — careers and self-concept fused with institutional indispensability -
 *   regional_safety_planning_bureaus: secondary beneficiary and
 *   ceremonial-track administrator (organized/constrained) — collects budgets
 *   for plans and drills, inherits front-line risk - lowland_residents:
 *   primary target (powerless/constrained) — bear concentrated exceedance
 *   risk after transferring self-protection to the center -
 *   local_first_responders: target (organized/trapped) — face the first hours
 *   of any failure without retained flood competence -
 *   flood_insurance_underwriters: incidental beneficiary
 *   (institutional/arbitrage) — prices risk against the assurance signal
 *   without maintaining competence - citizen_preparedness_advocates: excluded
 *   voice (moderate/mobile) — argues for distributed preparedness from
 *   outside the negotiating table - disaster_research_community: analytical
 *   observer (analytical/analytical) — audits the competence-ceremony gap
 *   comparatively
 *
 * KEY AGENTS:
 *   - specialized_water_agencies: agenda_setter and primary beneficiary (institutional/arbitrage) — administers the stratification and collects levy revenue, mandate expansion, and definitional authority
 *   - water_engineering_professionals: beneficiary (organized/identity_locked) — careers and self-concept fused with institutional indispensability
 *   - regional_safety_planning_bureaus: secondary beneficiary and ceremonial-track administrator (organized/constrained) — collects budgets for plans and drills, inherits front-line risk
 *   - lowland_residents: primary target (powerless/constrained) — bear concentrated exceedance risk after transferring self-protection to the center
 *   - local_first_responders: target (organized/trapped) — face the first hours of any failure without retained flood competence
 *   - flood_insurance_underwriters: incidental beneficiary (institutional/arbitrage) — prices risk against the assurance signal without maintaining competence
 *   - citizen_preparedness_advocates: excluded voice (moderate/mobile) — argues for distributed preparedness from outside the negotiating table
 *   - disaster_research_community: analytical observer (analytical/analytical) — audits the competence-ceremony gap comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.48).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention: Live Technical Core, Ceremonial Societal Memory").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster-preparedness/institutional-memory").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, 'ec9421ed-bff8-4daf-9897-76a97351e6a8').
narrative_ontology:cs_kernel_codification('ec9421ed-bff8-4daf-9897-76a97351e6a8', implicit).
narrative_ontology:cs_authority_grounding('ec9421ed-bff8-4daf-9897-76a97351e6a8', expertise).
narrative_ontology:cs_interpretation_layer_present('ec9421ed-bff8-4daf-9897-76a97351e6a8').
narrative_ontology:cs_reading_relation('ec9421ed-bff8-4daf-9897-76a97351e6a8', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('ec9421ed-bff8-4daf-9897-76a97351e6a8', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_axiom('ec9421ed-bff8-4daf-9897-76a97351e6a8', foundational, assessment_must_track_tiered_competence).
narrative_ontology:cs_axiom_status(assessment_must_track_tiered_competence, holdable).
narrative_ontology:cs_axiom_grounding('ec9421ed-bff8-4daf-9897-76a97351e6a8', assessment_must_track_tiered_competence, empirically_contingent).
narrative_ontology:cs_axiom('ec9421ed-bff8-4daf-9897-76a97351e6a8', foundational, concentration_requires_distributed_counterweight).
narrative_ontology:cs_axiom_status(concentration_requires_distributed_counterweight, holdable).
narrative_ontology:cs_axiom_grounding('ec9421ed-bff8-4daf-9897-76a97351e6a8', concentration_requires_distributed_counterweight, instrumental).
narrative_ontology:cs_reference_frame('ec9421ed-bff8-4daf-9897-76a97351e6a8', dual_track_complementary_retention).
narrative_ontology:cs_drift_state('ec9421ed-bff8-4daf-9897-76a97351e6a8', contemporary_post_limburg_2021, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec9421ed-bff8-4daf-9897-76a97351e6a8', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_water_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, regional_safety_planning_bureaus).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, flood_insurance_underwriters).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, lowland_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_first_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_engineering_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rijkswaterstaat and the water boards design, inspect, and operate the national flood-defense system and hold statutory monopoly on technical water authority. They receive water-board levies and national budget, define what counts as adequate preparedness, and have absorbed successive new mandates (climate adaptation, spatial planning) that expand their remit. Their continuity, staffing rationale, and definitional authority depend on remaining the indispensable competent core; exit for them means restructuring mandates and portfolios, which they have repeatedly done successfully.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_water_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, specialized_water_agencies, beneficiary).

% Engineers, dike masters, and inspectors trained through a dedicated pipeline whose careers, standing, and self-concept are built on being the protectors of the lowlands. Their professional identity is fused with the institutions' indispensability: acknowledging that part of the preparedness edifice is ceremonial would indict their own life's work. Leaving the cadre means abandoning not just employment but professional selfhood; retirement is the only exit most take.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_engineering_professionals, beneficiary,
    organized, biographical, identity_locked, national).

% Safety regions and municipal crisis-planning departments run the ceremonial track: scripted annual exercises, contingency plan documents, commemorative events, and awareness campaigns. They draw budgets and headcount justification from the ceremonial calendar and administer it, but they would inherit the front line in an exceedance event their scripting never rehearsed. They cannot opt out of statutory crisis duties, and attempts to build independent operational depth are absorbed into campaign formats or defunded.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, regional_safety_planning_bureaus, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, regional_safety_planning_bureaus, agenda_setter).

% People living below sea level or along the rivers who have transferred their self-protection role to the state under an assurance narrative. Most cannot name their evacuation route, warning threshold, or first-hour actions; their preparedness participation amounts to attending commemorations and receiving leaflets. Moving away from family, housing, and livelihood tied to the lowlands is costly, so they remain in place bearing the concentrated risk that distributed preparedness would have diffused.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, lowland_residents, payer,
    powerless, biographical, constrained, national).

% Municipal fire brigades and regional response units who would face the first hours of any breach or exceedance flood. Their flood-specific equipment and drilling are thin because the assurance narrative assigns them a supporting role behind the expert core, and their post-2021 requests for scenario-based training competed unsuccessfully with commemorative budgets. They cannot decline the response obligation when the event arrives; they are bound to the outcome whatever their preparation.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_first_responders, payer,
    organized, biographical, trapped, regional).

% Insurers price flood risk against the signal of expert-managed protection. The appearance of comprehensive institutional protection lets them exclude or tightly limit flood coverage at low cost and avoid full catastrophic pricing, collecting premiums on the assurance without maintaining any preparedness competence themselves. They can reprice or withdraw by portfolio adjustment at any time, an exit unavailable to the residents whose risk they price.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, flood_insurance_underwriters, beneficiary,
    institutional, biographical, arbitrage, national).

% Researchers, community organizers, and post-2021 critics arguing for distributed preparedness: neighborhood flood wardens, drills with genuine uncertainty, evacuation literacy in schools. They sit outside the table where preparedness policy is negotiated among ministries, agencies, water boards, and safety regions; their proposals are absorbed as awareness campaigns, defunded, or reframed as criticism of the experts rather than additions to the system. They can publish, move, and advocate elsewhere, which is where their influence goes.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, citizen_preparedness_advocates, excluded,
    moderate, biographical, mobile, national).

% Academic and comparative-governance researchers who audit the gap between exercised competence and performed memory, benchmarking the Dutch case against community-preparedness traditions elsewhere and against post-event inquiries. They see the full two-track structure, publish findings that neither fund nor staff anything, and constitute the analytical seat from which the stratification is visible as a single system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, disaster_research_community, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, specialized_water_agencies).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating hydraulic engineering, dike design and inspection, storm-surge barrier operation, and hydrological monitoring in permanent specialized institutions solves a genuine scale-and-continuity problem: delta defense requires multi-decade expertise, capital works beyond any locality's reach, and uniform safety standards that scattered actors cannot sustain.
% TRANSFER_FUNCTION: Moves the duty of preparedness from society at large to the specialized institutions: competence, authority, and levy revenue flow to the center; residents transfer their self-protection role and receive protection-as-service; legitimacy flows back outward through commemoration, campaigns, and scripted exercises. Risk-bearing moves the opposite way — residual and exceedance risk concentrates on residents and first responders.
% ABSENT_VOICES: Residents and citizen-preparedness advocates are absent from the table where preparedness policy is set (ministries, agencies, water boards, safety regions). The people whose distributed resilience is converted into ceremony appear only as audience — commemoration turnout, survey respondents. Advocates who object are absorbed into campaign roles or excluded from funding; no seat at the design table represents the periphery's interest in keeping its own capacity.
% DISAPPEARANCE_RATIONALE: If the stratified arrangement vanished overnight, the specialized institutions' mandates, levy base, and staffing rationale would collapse with nothing pre-built to replace them; residents' current non-preparedness would meet immediate exposure with no distributed capacity to fall back on; safety regions would face response obligations their ceremonial preparation never equipped them for. Who holds competence and who bears risk would have to be renegotiated from scratch — the world rearranges.
% FOUNDING_PROBLEM: After the 1953 North Sea Flood killed 1,836 people in the Netherlands, the founding problem was how a densely populated delta state could guarantee protection against storm-surge flooding at national scale — answered with the Delta Works and a permanent corps of specialized water institutions.
% FOUNDING_PROBLEM_CORROBORATION: The historical record and international delta-engineering literature corroborate that the founding problem was real and that the centralized answer performed at its design standard. On current status, the Dutch Safety Board's investigations following the 2021 Limburg floods — an independent body outside the benefiting parties — documented preparedness gaps in terrain outside the core defense mission; insurance-sector risk assessments and comparative disaster-governance research corroborate that parts of the arrangement now persist on institutional momentum. No source outside the benefiting parties attests that the founding problem stands unchanged since 1953.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope from this reading's own lights: the arrangement possesses a genuine coordination function (centralized delta-defense competence solves a real scale-and-continuity problem no locality could) AND asymmetric extraction through the same structure (societal preparedness capacity is converted into ceremony while risk concentrates on those left with neither). Extraction is 0.58: the referent is the standing stratified arrangement assessed by this reading — the live core delivers real protection (which damps epsilon below snare territory), but the net transfer off the periphery is capacity and risk, not nothing. Suppression is 0.48 as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine): nothing legally bans citizen preparedness, but the alternative is crowded out by statute-shaped funding, absorbed as awareness campaigning, and deflated by a managed assurance narrative. Theater_ratio is 0.44 system-wide — the technical core is heavily functional while the peripheral track alone would measure roughly 0.7 theatrical; the scalar reports the weighted whole. Accessibility_collapse is 0.52: once the stratification is understood, alternatives (community flood wardens, uncertain-scenario drilling, evacuation literacy) are not impossible but are systematically absorbed, defunded, or reframed. Resistance is 0.38: post-2021 Limburg criticism, municipal grumbling about dependency, and advocacy pressure are real but lack leverage over the statutory core. The temporal series run on one shared eight-point grid (every tracked metric authored at every point, T=0..70, roughly 1953-2023): extractiveness climbs as lived memory recedes generationally; theater_ratio climbs as the commemorative calendar institutionalizes; suppression_requirement climbs because the story specifically tracks the maturation of narrative-management machinery — early decades ran on inherited high-trust (little active suppression needed), later decades require actively produced reassurance, controlled risk communication, and investment in commemoration to hold the periphery in its audience role.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda_setter seat (specialized_water_agencies) the arrangement is a coordination triumph it built, staffs, and continuously validates — coordination-dominant classification expected. From the payer seats (lowland_residents, local_first_responders) the same structure operates as capacity taken and risk returned — extraction-dominant. The professional cadre's identity lock bends even insider self-assessment: auditing the system as partly hollow would indict their own life's work, so the cadre seat computes more benign than an external audit of identical operations. The insurance seat experiences the arrangement as nearly free optionality — protection signal without competence duty — and computes the lowest extraction of any collecting seat. The engine derives these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. specialized_water_agencies sit near the full-beneficiary end (d near 0.05-0.1): they collect levies, mandates, and definitional authority, with arbitrage-grade restructuring options. water_engineering_professionals derive similarly low d, amplified toward stability by identity lock — exit would cost them their professional selves. flood_insurance_underwriters derive low d through arbitrage exit: they can reprice or withdraw by portfolio adjustment. regional_safety_planning_bureaus are the genuinely mixed seat: declared beneficiary (they collect budgets and headcount justification from the ceremonial calendar) but true position sits nearer d~0.3 because they inherit front-line failure in exceedance events. That correction is documented here rather than authored as a directionality_override deliberately: overrides key on the power atom, and local_first_responders share the 'organized' atom while needing a high target-side d — a single organized-atom override would corrupt both derivations. lowland_residents (powerless, constrained exit) and local_first_responders (organized, trapped) derive near the full-target end (d roughly 0.85-0.95): they bear the transferred risk with the least mobility. The observer seat carries no material directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting a delta nation against 1953-class storm surge — was real and has been engineered down to its design standard; the arrangement nonetheless persists with expanding mandates (climate adaptation, spatial planning), and the ceremonial track exhibits textbook mandatrophy symptoms: rituals outliving the memory they commemorate, exercises rehearsing scripts rather than decisions, campaigns substituting for capacity. Founding_problem_status is authored 'contested' rather than 'dead' because the parties genuinely dispute it — the establishment attests the problem is live and evolving (sea-level rise, extreme rainfall), while the Safety Board's post-2021 findings and comparative research attest that large parts of the current arrangement serve institutional continuity beyond the original problem. Because status is contested rather than dead, the mismatch consumer finds no clean zombie flag here — but the periphery's theater trajectory is the drift signature worth watching. The tangled_rope classification prevents mislabeling in both directions: calling the whole thing a snare erases the live core that genuinely protects millions; calling it a rope erases the conversion of societal memory into audiencehood. Both truths stay load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the stratified description accurate, or does the truth of preparedness retention sit wholly with the husk_reading (everything is memorial performance) or the competence_reading (everything is live competence-preserving practice)?',
    'Comparative audit of drill and exercise outcomes against ceremony content across institutional tiers: score exercises by whether decisions under uncertainty were rehearsed or scripts recited, separately for Rijkswaterstaat/water-board operations and for municipal/safety-region activities.',
    'If the husk_reading wins, epsilon rises sharply toward pure-extraction territory and the tangled_rope claim collapses; if the competence_reading wins, epsilon falls toward coordination-cost levels and the victim declarations lose their ground. This story''s classification stands or falls with the stratification finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Between-readings contest over whether retention is stratified, uniformly live, or uniformly ceremonial.').

omega_variable(
    centralization_fragility_question,
    'Does concentrating technical competence actually create a single point of failure (the reading''s victim claim), or is the core''s depth sufficient compensation such that distributed atrophy costs little?',
    'Stress-test analysis of exceedance scenarios (multi-breach storms, compound fluvial-pluvial events like July 2021 in Limburg) quantifying how much peripheral capacity would have been required and whether the core could scale to cover it.',
    'If the core suffices across credible exceedance scenarios, the residents'' victim status weakens and the arrangement drifts toward rope; if not, effective extraction exceeds the authored 0.58 and the single-point-of-failure framing hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralization_fragility_question, empirical, 'Whether centralized expertise is fragility or adequate redundancy.').

omega_variable(
    deference_internalization_ambiguity,
    'Is the periphery''s non-preparedness structural (crowded out, unfunded, unmandated) or internalized (high-trust deference and learned reliance that would persist even if resources and mandate were offered)?',
    'Post-intervention trajectory: pilot programs offering communities real resources, mandate, and training; if competence regrows, suppression was structural; if uptake stays low while the assurance narrative persists, the deference is internalized.',
    'If internalized, effective suppression is higher than the structural 0.48 suggests — residents carry the deference with them — and fixing_cost rises further because cultural reversal, not budget reallocation, becomes the binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_internalization_ambiguity, empirical, 'Structural versus internalized mechanism behind peripheral preparedness atrophy.').

omega_variable(
    ceremony_residual_function,
    'Do the commemorations, awareness weeks, and scripted drills retain a residual floor of genuine societal memory that the hybrid reading''s ''ceremonial'' label discounts?',
    'Longitudinal survey and behavioral testing of residents exposed versus unexposed to commemorative and campaign activity: does ceremonial contact predict any actionable knowledge (warning thresholds, evacuation behavior) above zero?',
    'If a real memory floor exists, theater_ratio overstates peripheral hollowness and part of the ceremonial track is functional coordination; if not, the periphery is closer to pure performance and the husk_reading gains ground within this reading''s own territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremony_residual_function, conceptual, 'Whether the ceremonial track preserves any live memory at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(prep_tr_t60, preparedness_retention__hybrid_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(prep_tr_t70, preparedness_retention__hybrid_reading, theater_ratio, 70, 0.44).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(prep_be_t60, preparedness_retention__hybrid_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(prep_be_t70, preparedness_retention__hybrid_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__hybrid_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(prep_su_t60, preparedness_retention__hybrid_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(prep_su_t70, preparedness_retention__hybrid_reading, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'preparedness retention' decomposes into three structurally distinct claims per the epsilon-invariance principle. competence_reading (upstream, establishment-endorsed) describes a uniformly live system; husk_reading (critical) describes a uniformly ceremonial one; this hybrid_reading describes stratification — live core, ceremonial periphery. The siblings are not observables of one constraint: their epsilon values, victim sets, and failure modes differ, so each is authored as its own story. This reading links to both because each sibling's claim, if established, would dissolve this reading's central stratification premise; empirically the upstream competence_reading is cited BY the establishment as evidence against the husk_reading, while the hybrid reading's tiered-audit resolution mechanism is the proposed arbiter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
