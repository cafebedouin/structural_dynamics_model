% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading — Categorical Prohibition on State Racial Classifications
 *   domain: constitutional law / education policy / civil rights
 *
 * SUMMARY:
 *   This story instantiates the colorblind reading of the
 *   equal_protection_kernel: the Fourteenth Amendment categorically forbids
 *   state use of racial classifications regardless of purpose. The reading's
 *   structural delta is total — any race-conscious admissions policy is per
 *   se unconstitutional, all applicants are treated identically under formal
 *   equality, historically excluded groups lose the remedial pathway, and the
 *   state acquires no obligation to address the effects of past
 *   discrimination. Operatively, the reading ran as dictum and plurality
 *   language from 1978 (Bakke), hardened through strict scrutiny of
 *   set-asides (Croson 1989, Adarand 1995), narrowed the deference window
 *   (Grutter 2003, Fisher 2013/2016), struck down voluntary K-12 integration
 *   plans (Parents Involved 2007), and became categorical for admissions in
 *   2023. KEY AGENTS (by structural relationship):
 *   federal_supreme_court_majority — agenda setter (institutional/arbitrage),
 *   authors and revises the doctrine; colorblind_movement_litigants — primary
 *   beneficiary (organized/mobile), strategic litigation machine collecting
 *   precedent and resources; historically_excluded_minority_applicants —
 *   primary target (powerless/trapped), bear the foreclosed remedial pathway;
 *   status_quo_advantaged_applicants — secondary beneficiary
 *   (moderate/mobile), retain seats under race-neutral metrics;
 *   race_neutral_preference_holders — incidental beneficiary
 *   (powerful/arbitrage), legacy/donor/geographic channels untouched;
 *   public_universities_admissions_offices — constrained intermediary
 *   (institutional/constrained), compliance costs and lost instrument;
 *   integration_planning_school_districts — secondary target
 *   (institutional/constrained); affected_community_members_without_standing
 *   — excluded seat; legal_academy_analysts — analytical observer.
 *   Claim/metric independence is deliberate here in a specific way: the
 *   claimed_type (tangled_rope) is my structural assessment of the
 *   categorical rule as it actually operates, while epsilon is authored
 *   reading-indexed — from the colorblind frame's own lights, per the
 *   kernel-reading epsilon-referent rule — over the fixed referent of the
 *   operative rule. The frame sees formal equality as neutral and registers
 *   only a small residual burden (interests the frame classifies as cessation
 *   of an illegitimate benefit rather than taking); the sibling stories share
 *   that referent and author materially different epsilon.
 *
 * KEY AGENTS:
 *   - federal_supreme_court_majority: agenda setter (institutional/arbitrage) — authors, revises, and enforces the categorical rule; its exit is doctrinal self-revision
 *   - colorblind_movement_litigants: primary beneficiary (organized/mobile) — multi-decade strategic litigation campaign collecting precedent, members, and funding
 *   - historically_excluded_minority_applicants: primary target (powerless/trapped) — bear the closure of the remedial pathway; cannot exit the applicant pool, the jurisdiction, or their group's history
 *   - status_quo_advantaged_applicants: secondary beneficiary (moderate/mobile) — credential profiles prevail under the locked-in race-neutral metrics
 *   - race_neutral_preference_holders: incidental beneficiary (powerful/arbitrage) — legacy, donor, and geographic preference channels the rule does not touch
 *   - public_universities_admissions_offices: constrained intermediary (institutional/constrained) — rewrite procedures, absorb compliance and litigation exposure, lose a tool they valued
 *   - integration_planning_school_districts: secondary target (institutional/constrained) — voluntary integration plans invalidated; manage resegregation with demographic proxies
 *   - affected_community_members_without_standing: excluded seat — live inside the rule's effects with no standing and no seat in the interpretive process
 *   - legal_academy_analysts: analytical observer — maps genealogy, coherence, and consequences; sees the sibling readings' standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.18).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.62).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Colorblind Reading — Categorical Prohibition on State Racial Classifications").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional law / education policy / civil rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d').
narrative_ontology:cs_kernel_codification('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', fixed_text).
narrative_ontology:cs_authority_grounding('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', lineage).
narrative_ontology:cs_interpretation_layer_present('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d').
narrative_ontology:cs_reading_relation('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', foundational, racial_classifications_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', racial_classifications_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', foundational, benign_and_invidious_classifications_indistinguishable).
narrative_ontology:cs_axiom_status(benign_and_invidious_classifications_indistinguishable, holdable).
narrative_ontology:cs_axiom_grounding('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', benign_and_invidious_classifications_indistinguishable, deontological).
narrative_ontology:cs_reference_frame('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', reconstruction_era_colorblind_founding_understanding).
narrative_ontology:cs_drift_state('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', post_sffa_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b69777c-e3a9-4d86-a64d-bdbd64ad1f8d', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_movement_litigants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, status_quo_advantaged_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, race_neutral_preference_holders).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, integration_planning_school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, public_universities_admissions_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and revisits the doctrine that state actors may not classify by race for any purpose. A five-justice coalition defines the rule's reach, polices lower-court compliance through certiorari and summary disposition, and can reframe precedent — it moved the doctrine from individualized balancing to a categorical prohibition. Its exit is doctrinal self-revision; no external enforcer binds it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_supreme_court_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Runs a multi-decade strategic litigation campaign against race-conscious admissions, contracting set-asides, and school integration plans. Collects favorable precedent, member growth, and fundraising momentum with each win, and pivots to fresh targets — corporate diversity programs, grant conditions — as new fronts open. Mobile across venues and defendant classes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_movement_litigants, beneficiary,
    organized, generational, mobile, national).

% Applies to selective institutions and public programs whose gatekeeping the rule now fixes to race-neutral criteria. Cannot leave the applicant pool, relocate out of the doctrine's jurisdiction in any practically meaningful way, or alter the group history the rule renders unaddressable. Bears the closure of the remedial pathway and the shortfall of race-neutral proxies against documented exclusion.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants, payer,
    powerless, biographical, trapped, national).

% Competes under the race-neutral metrics the rule locks in, and its credential profiles — test scores, school quality, continuity of preparation — tend to prevail under those metrics. Keeps the selective seats that race-conscious review would have redistributed. Individually uninvolved in setting the rule, but positioned to receive its day-to-day operation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, status_quo_advantaged_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Holds advantages flowing through channels the categorical rule does not touch: legacy status, donor relationships, geographic distribution schemes, athletic recruitment. Because the rule prohibits only race-conscious adjustment, these channels continue undisturbed, and their holders' relative position is preserved by the rule's selectivity.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, race_neutral_preference_holders, beneficiary,
    powerful, biographical, arbitrage, national).

% Rewrites admissions procedures, retires race-conscious review it judged pedagogically valuable, and absorbs compliance monitoring, general-counsel review of every proxy, and litigation exposure. Cannot exit the constitutional regime governing it; offsets some losses through income bands, adversity essays, and percentage plans drawn inside the rule's lines.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, public_universities_admissions_offices, payer,
    institutional, generational, constrained, national).

% Drew voluntary integration plans — controlled choice, race-conscious transfer and magnet rules — and saw them invalidated. Manages resegregation consequences with demographic proxies, bounded by local housing patterns it cannot control, and remains subject to further suit if proxies are read as classifications.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, integration_planning_school_districts, payer,
    institutional, generational, constrained, regional).

% Lives inside the rule's effects — students choosing schools, families weighing moves, communities watching campuses resegregate — but holds no litigation standing and no seat in the interpretive process. Its interests reach the doctrine only through counsel selected by organizational litigants and through amicus filings it does not control.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, affected_community_members_without_standing, excluded,
    powerless, biographical, trapped, national).

% Maps the reading's genealogy, internal coherence, and downstream consequences; publishes critiques and defenses; runs comparative analyses against other jurisdictions' equality clauses. Holds no operational stake and observes the full structure, including the standing of the sibling readings.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, legal_academy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, status_quo_advantaged_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, administrable rule of formal equality across all state actors: every applicant and citizen evaluated without regard to race, eliminating case-by-case judicial balancing of racial purposes, giving institutions a bright compliance line, and preventing spiraling inter-group competition for state-administered preferences.
% TRANSFER_FUNCTION: Moves allocation discretion away from institutions that would weigh race and reallocates the incidence of educational opportunity toward applicants whose profiles prevail under race-neutral metrics — preserving the existing distribution of advantage against race-conscious correction — while moving litigation risk and compliance cost onto institutions that attempt race-conscious policy.
% ABSENT_VOICES: Communities bearing the rule's costs — prospective students from historically excluded groups, families in districts whose integration plans were struck down — hold no seat in the interpretive process; their interests enter only through counsel chosen by organizational litigants with standing and through amicus briefs. The antisubordination tradition speaks in dissents and scholarship, outside the operative coalition.
% DISAPPEARANCE_RATIONALE: If the categorical rule vanished overnight, universities would reinstate race-conscious review (several announced intent to do so the moment the doctrine shifted), districts would revive integration planning, a litigation wave would restart immediately in both directions, and the distribution of selective seats would begin redistributing within admission cycles — the education-opportunity architecture reorganizes around whichever reading next commands a majority.
% FOUNDING_PROBLEM: The clause was built to protect newly freed slaves from state-engineered caste: Black Codes, denial of contract and court access, exclusion from the political community. The colorblind reading frames that founding problem as the need to prevent government from ever distributing burdens or benefits by race, because racial distribution is the engine of caste.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists on both sides and is genuinely disputed. Outside the benefiting parties, Reconstruction historiography (Foner, Kaczorowski) attests from legislative records that the 39th Congress enacted expressly race-conscious remedial measures alongside the Clause — corroborating that the founding problem was caste-specific subordination rather than classification per se, and cutting against this reading's genealogy. On the other side, Harlan's Plessy formulation and the originalist literature attest that a colorblind understanding of the clause has been continuously articulated since 1896. No party outside the dispute adjudicates it; the status is contested, not settled by either genealogy.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).
:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.18 because the referent is the operative categorical rule assessed by the colorblind reading's own lights: within that frame, identical treatment extracts nothing, and the only registered burden is the acknowledged loss to interests the frame deems impermissibly pursued — hence low but nonzero, rising from 0.10 to 0.18 as the reading consolidated from partial (diversity exception live under Bakke) to categorical (2023), sweeping more state action into the prohibition and widening the surface of burdened interests even on the frame's own accounting. Suppression is 0.62 as a raw structural property — unscaled by power or scope: the rule persists through active judicial invalidation of contrary state and institutional preferences plus compelled compliance machinery (procedure rewrites, general-counsel review of every proxy), not through voluntary uptake. Theater_ratio is 0.20 and falling across the interval: early on, colorblind language was largely ceremonial (Harlan's dictum invoked in opinions doing other work), while the post-2023 rule bites directly, so performative maintenance has shrunk as functionality rose. Accessibility_collapse is 0.55: the specific race-conscious alternative collapses almost completely once the rule is understood, but adjacent race-neutral substitutes (income bands, percentage plans, adversity essays) remain open, so alternatives collapse only partly overall. Resistance is 0.65: four-justice dissents, sustained scholarly opposition, institutional edge-testing of proxies, and continued advocacy organization all contest the rule. The measurement series runs on one shared nine-point grid (1978, 1989, 1995, 2003, 2007, 2013, 2016, 2023, 2025) with all three tracked metrics authored at every point; suppression_requirement is tracked because the story's central dynamic is an enforcement ratchet — the reading's coercive machinery matured from a single plurality vote to categorical prohibition with compliance monitoring. The 2003 dip models the Grutter deference window before the ratchet resumed. Coalition note: the trapped rating on the primary payer seat understates latent class-level coalition power (electoral, institutional, and amicus-scale coordination), which is the main channel through which the payer seat's position could change.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very differently. From the agenda-setter seat the rule is a neutral constitutional command it authored and polices; from the litigant seat it is vindication; from the payer seats the same structure operates as the closure of remedy layered on top of untouched rival preference channels. Same-power divergence: universities and school districts both sit at institutional power with constrained exit, but universities have endowments, legal staff, and proxy-engineering capacity that soften their burden, while districts are bound by housing patterns they cannot control — equal nominal standing, unequal practical position. Identity-lock dynamics bind the litigant seat: the colorblind reading functions as professional and ideological identity for the legal movement that produced it — careers, networks, and worldview are constituted through the doctrine, making exit unthinkable independent of any case-level calculus; if that identity frame broke, the enforcement supply behind the rule would thin rapidly. The excluded seat matters for consensus provenance: the unanimity of the operative framework partly reflects that the people living inside its effects never held a seat in the room where its meaning was fixed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. colorblind_movement_litigants sit near the beneficiary pole (collect precedent and resources, mobile exit across venues). status_quo_advantaged_applicants sit near-beneficiary (receive the rule's day-to-day material operation: seats not redistributed). race_neutral_preference_holders sit nearest the beneficiary pole with arbitrage-grade positioning — the rule does not touch their channels at all, so their advantage is preserved by the rule's selectivity rather than taxed by it. historically_excluded_minority_applicants sit near the full-target end: trapped exit (cannot leave the pool, the jurisdiction, or their history) plus the rule's binding of their group's history as unaddressable places them at maximum exposure. integration_planning_school_districts and public_universities_admissions_offices derive mid-to-high directionality — they pay compliance and bear lost-function costs with constrained exit. The court's seat is computed from its administration role rather than from a beneficiary declaration; no directionality_overrides are authored because the role-plus-exit derivation captures every seat's relationship, and the schema's override mechanism keys on power atoms, which would be too blunt to differentiate the two institutional payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both halves of this constraint visible. Mislabeling it as pure coordination would erase the documented asymmetry — the remedial pathway closes for one side while legacy, donor, and geographic channels stay open — which is exactly the extraction the antisubordination tradition alleges. Mislabeling it as pure extraction would erase the genuine coordination function — a bright-line, administrable rule of formal equality that solves real collective-action problems (case-by-case racial balancing, inter-group preference competition, majoritarian abuse of unpopular minorities) — which is exactly the value the colorblind frame claims. On mandatrophy proper: the founding problem (preventing state-engineered caste) is contested rather than dead — the frame holds the mandate eternally live (any racial typing risks caste), while the sibling readings hold the mandate betrayed when neutrality preserves inherited hierarchy. Because founding_problem_status is 'contested' rather than 'dead', the dead-mandate-plus-world_rearranges mismatch does not fire; the constraint is not a zombie administering a corpse mandate, it is a live fight over what the mandate is. The temporal series supports this: falling theater and rising enforcement show a constraint becoming more functional and more coercive simultaneously — atrophy is not the failure mode here; hardening is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equal_protection_kernel_reading_contestation,
    'This constraint is one reading of the equal_protection_kernel; how would the sibling readings (remedial_reading, antisubordination_reading) restructure the constraint''s victim set, beneficiary set, and epsilon?',
    'The sibling stories instantiate the same kernel with their own beneficiary/victim declarations and their own reading-indexed epsilon over the shared referent (the operative categorical rule); cross-reading comparison locates the disagreement in the clause''s object — classification per se versus caste-like subordination versus remediable documented exclusion.',
    'Under the remedial sibling, race-conscious admissions become the coordinated arrangement and the categorical ban becomes the extractive deviation; under the antisubordination sibling, the victim set expands to everyone harmed by hierarchy-preserving neutrality. This story''s classification holds only within the colorblind frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equal_protection_kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested constitutional kernel; sibling readings would produce materially different constraints from the same text.').

omega_variable(
    founding_generation_original_meaning,
    'Did the 1868 framers understand the Fourteenth Amendment as a categorical colorblind rule, or as a caste-targeted guarantee compatible with race-conscious remedial legislation?',
    'Legislative history of the 39th Congress, authorization records for the Freedmen''s Bureau and early civil rights acts, and Reconstruction historiography weighing the framers'' endorsement of expressly race-conscious remedial measures.',
    'If the framers endorsed race-conscious remediation, the colorblind reading''s lineage claim — its authority_grounding — is refuted at the source, eroding its legitimacy structure and shifting classification pressure toward extraction-grounded authority; if confirmed, the reading''s reference frame is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_generation_original_meaning, empirical, 'Whether the reading''s genealogical warrant survives professional historiography of the founding generation.').

omega_variable(
    categorical_rule_gain_capture,
    'Where do the material gains of the categorical rule actually accrue — broadly across applicants who value formal equality, or concentrated among legacy-, geographic-, and credential-advantaged pools?',
    'Longitudinal admissions-composition data at selective institutions after the 2023 ruling, decomposing seat redistribution by applicant category (legacy status, geography, income band, test-score band).',
    'Concentrated capture sharpens the extraction asymmetry and pressures classification from tangled_rope toward snare; broad diffusion strengthens the rope-side coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_rule_gain_capture, empirical, 'Whether the rule''s gains are captured by identifiable advantaged seats or diffuse across the applicant pool.').

omega_variable(
    stigma_symmetry_axiom_warrant,
    'Does the reading''s foundational axiom — that benign and invidious racial classifications are legally indistinguishable because both inflict stigma — survive the empirical finding that majority-race beneficiaries of a classification do not experience stigma analogous to that experienced by subordinated groups?',
    'Social-psychological stigma measurement across classification types (classifications burdening majority-race applicants versus classifications burdening historically subordinated groups), replicated across cohorts and institutions.',
    'Confirmed asymmetry undermines the deontological grounding of the categorical axiom, weakening this reading''s distinction from the remedial sibling and pressuring its foreclosure claim; refuted asymmetry stabilizes the axiom and the reading''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_symmetry_axiom_warrant, empirical, 'Empirical warrant for the axiom that all racial classifications carry equivalent dignitary harm.').

omega_variable(
    race_neutral_substitute_adequacy,
    'Can race-neutral proxies — income preferences, percentage plans, adversity essays — achieve the compositional outcomes the prohibited race-conscious tools achieved?',
    'Post-2023 natural experiment: compare composition trajectories at institutions adopting aggressive race-neutral proxies against institutions that did not, controlling for applicant-pool changes.',
    'If substitutes suffice, the rule suppresses a method rather than an outcome and the burden on the payer seats is lower than the raw prohibition suggests; if they fail, the rule suppresses the outcome itself, raising effective extraction on historically excluded applicants and sharpening the tangled-rope asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_neutral_substitute_adequacy, empirical, 'Whether the constraint''s suppression falls on methods or on outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.38).
narrative_ontology:measurement(equa_tr_t1989, equal_protection_kernel__colorblind_reading, theater_ratio, 1989, 0.33).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_kernel__colorblind_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement(equa_tr_t2007, equal_protection_kernel__colorblind_reading, theater_ratio, 2007, 0.24).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__colorblind_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__colorblind_reading, theater_ratio, 2016, 0.21).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(equa_tr_t2025, equal_protection_kernel__colorblind_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.1).
narrative_ontology:measurement(equa_be_t1989, equal_protection_kernel__colorblind_reading, base_extractiveness, 1989, 0.13).
narrative_ontology:measurement(equa_be_t1995, equal_protection_kernel__colorblind_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.15).
narrative_ontology:measurement(equa_be_t2007, equal_protection_kernel__colorblind_reading, base_extractiveness, 2007, 0.16).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__colorblind_reading, base_extractiveness, 2013, 0.16).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__colorblind_reading, base_extractiveness, 2016, 0.17).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.18).
narrative_ontology:measurement(equa_be_t2025, equal_protection_kernel__colorblind_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(equa_su_t1989, equal_protection_kernel__colorblind_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement(equa_su_t1995, equal_protection_kernel__colorblind_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.48).
narrative_ontology:measurement(equa_su_t2007, equal_protection_kernel__colorblind_reading, suppression_requirement, 2007, 0.55).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__colorblind_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__colorblind_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.75).
narrative_ontology:measurement(equa_su_t2025, equal_protection_kernel__colorblind_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Equal Protection' decomposes into three structurally distinct constraints — the colorblind, remedial, and antisubordination readings of the same kernel text. Each has its own epsilon (reading-indexed over the shared referent of the operative doctrine), its own victim set, and its own failure modes; forcing one story to span all three would violate epsilon-invariance. This story (colorblind) links to both siblings. Upstream/downstream: the colorblind reading currently dominates the operative doctrine, so it structurally influences the siblings' practical availability even though its logical relation to each is foreclosure within a single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
