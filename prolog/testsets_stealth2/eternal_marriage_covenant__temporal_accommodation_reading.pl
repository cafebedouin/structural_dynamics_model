% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Post-Manifesto Plural Marriage Suspension (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   In September 1890 Wilford Woodruff issued the Manifesto advising
 *   Latter-day Saints against contracting marriages forbidden by the law of
 *   the land. This story instantiates the temporal_accommodation_reading of
 *   the eternal_marriage_covenant kernel: the Manifesto suspends practice
 *   without renouncing doctrine; the eternal principle of D&C 132 remains
 *   valid; obedience to civil law takes precedence for the present; the
 *   doctrine lies dormant pending restoration when political constraints
 *   lift. The epsilon referent is the standing post-Manifesto arrangement —
 *   practice suspended, doctrine retained, civic obedience prioritized —
 *   assessed by this reading's own lights, which treat the suspension as
 *   legitimate revealed accommodation while acknowledging its real and
 *   growing burden on believers. Sibling readings
 *   (immutable_commandment_reading, prophetic_override_reading) are separate
 *   constraints with their own files; they are not averaged into this one.
 *   Claim and metrics are independent: claimed_type tangled_rope states my
 *   structural read (a genuine coordination function joined to asymmetric,
 *   actively enforced costs); the metrics describe the arrangement's actual
 *   operation as the record shows it. KEY AGENTS (by structural
 *   relationship): - first_presidency_and_twelve: Agenda setter
 *   (institutional/identity_locked) — issued and administers the suspension -
 *   institutional_church_corporation: Primary beneficiary
 *   (institutional/arbitrage) — collects survival, property, and statehood -
 *   federal_government_of_the_united_states: Secondary beneficiary
 *   (institutional/mobile) — collected compliance and closed the
 *   confrontation - general_church_membership: Beneficiary with payer
 *   underside (organized/constrained) — relief from siege, quiet doctrinal
 *   costs - devout_plural_marriage_believers: Primary target
 *   (powerless/identity_locked) — bear the heaviest ongoing burden -
 *   existing_plural_families: Target (powerless/trapped) — households left in
 *   legal and social limbo - fundamentalist_dissenters: Excluded voice
 *   (organized/identity_locked) — objected, were disciplined out -
 *   mormon_history_scholars: Analytical observer (analytical/analytical) —
 *   see the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.62).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.55).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Post-Manifesto Plural Marriage Suspension (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '293d6a4f-df14-43ad-9e2e-3a077034a26c').
narrative_ontology:cs_kernel_codification('293d6a4f-df14-43ad-9e2e-3a077034a26c', fixed_text).
narrative_ontology:cs_authority_grounding('293d6a4f-df14-43ad-9e2e-3a077034a26c', lineage).
narrative_ontology:cs_interpretation_layer_present('293d6a4f-df14-43ad-9e2e-3a077034a26c').
narrative_ontology:cs_reading_relation('293d6a4f-df14-43ad-9e2e-3a077034a26c', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('293d6a4f-df14-43ad-9e2e-3a077034a26c', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('293d6a4f-df14-43ad-9e2e-3a077034a26c', foundational, civic_obedience_supersedes_contested_practice).
narrative_ontology:cs_axiom_status(civic_obedience_supersedes_contested_practice, holdable).
narrative_ontology:cs_axiom_grounding('293d6a4f-df14-43ad-9e2e-3a077034a26c', civic_obedience_supersedes_contested_practice, deontological).
narrative_ontology:cs_axiom('293d6a4f-df14-43ad-9e2e-3a077034a26c', foundational, suspension_is_not_renunciation).
narrative_ontology:cs_axiom_status(suspension_is_not_renunciation, holdable).
narrative_ontology:cs_axiom_grounding('293d6a4f-df14-43ad-9e2e-3a077034a26c', suspension_is_not_renunciation, conventional).
narrative_ontology:cs_reference_frame('293d6a4f-df14-43ad-9e2e-3a077034a26c', covenant_intact_practice_suspended).
narrative_ontology:cs_drift_state('293d6a4f-df14-43ad-9e2e-3a077034a26c', contemporary_post_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('293d6a4f-df14-43ad-9e2e-3a077034a26c', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_corporation).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_government_of_the_united_states).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, general_church_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, devout_plural_marriage_believers).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, general_church_membership).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, twelfth_article_of_faith_civic_obedience).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, eternal_validity_of_new_and_everlasting_covenant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto and administer its terms: screen candidates for temple ordinances, discipline members entering new plural marriages, and issue clarifying statements including the 1904 Second Manifesto. They frame the suspension as revealed accommodation rather than surrender. Stepping outside their own offices would dissolve the very authority that issued the suspension, so they cannot exit the arrangement they govern.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, first_presidency_and_twelve, agenda_setter,
    institutional, generational, identity_locked, continental).

% Holds the church's property, charters, and legal personality. Under the Edmunds-Tucker Act it faced forfeiture of temples and assets; the suspension preserved its corporate existence, and it restructured its holdings under a corporation sole to secure them. It collects the arrangement's principal continuities: legal standing, Utah statehood, and uninterrupted institutional growth.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_corporation, beneficiary,
    institutional, generational, arbitrage, continental).

% Criminalized plural marriage, seized church property, and disenfranchised adherents until the practice stopped. With the Manifesto it obtained compliance without further prosecution, admitted Utah as a state in 1896, and closed the confrontation. It retains the capacity to resume enforcement at will, though it has had no occasion to for over a century.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government_of_the_united_states, beneficiary,
    institutional, generational, mobile, national).

% Gained relief from imprisonment raids, disfranchisement, and social siege. Bears the arrangement's quieter costs: reconciling taught doctrine with required practice, and loss of access to an ordinance many were taught was essential to the highest degree of salvation. Leaving the community carries severe social and theological cost, so most remain and reconcile.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, general_church_membership, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, general_church_membership, payer).

% Hold D&C 132 as eternal law bound up with their exaltation. The suspension bars them from entering plural marriage while their theology tells them the principle remains in force. Continuing secretly risks discipline; abandoning the hope compromises their conception of salvation; leaving the faith forfeits everything the covenant promised. They carry the arrangement's heaviest ongoing burden, and their stake extends past their own lifetimes.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, devout_plural_marriage_believers, payer,
    powerless, civilizational, identity_locked, continental).

% Households formed before 1890 remained legally unrecognized and socially exposed after the Manifesto. Husbands faced continued prosecution risk for existing relationships; wives held no recognized marital status; children carried stigma. Nothing in the arrangement dissolves or repairs these households; they persist in limbo across a generation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families, payer,
    powerless, generational, trapped, regional).

% Rejected the Manifesto's authority from the start, insisting the command could not be withdrawn by accommodation to civil power. Organized networks to continue sealings and marriages in Mexico, Canada, and underground, were disciplined and excommunicated, and eventually separated into distinct communities. They would speak against the arrangement at every turn; their exclusion defines its boundary.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters, excluded,
    organized, civilizational, identity_locked, continental).

% Study the episode from outside the faith's authority structure, working from diaries, court records, and meeting minutes. They observe the full structure — the federal pressure, the institutional calculus, the believers' burden, and the doctrinal bookkeeping that keeps the principle canonically alive while practice is impossible — and weigh the competing accounts of what the Manifesto was.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, mormon_history_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_corporation).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a head-on collision between two authorities the community recognizes as supreme: divine command (D&C 132) and federal criminal law. The priority rule — obedience to the law of the land takes precedence — lets the community keep both commitments in view, survive the state's campaign of seizures and imprisonments, and retain internal cohesion, solving a collective survival problem no individual member could solve alone.
% TRANSFER_FUNCTION: Moves compliance from believing practitioners to the federal state (foregone plural marriages, cessation of open practice); moves survival, property security, and statehood to the institutional church; moves reassurance back to the membership in the form of the doctrine's retained canonical status.
% ABSENT_VOICES: Fundamentalist dissenters objected and were present — then were disciplined out of the conversation, so the settled version of the arrangement reflects only voices that accepted it. Existing plural wives had no seat in the decision that defined their households' status. Both would still object; one group now lives outside the boundary the arrangement drew.
% DISAPPEARANCE_RATIONALE: If the suspension regime vanished overnight, the church's constitutional settlement with the state would reopen: the corporate form holding its property, the Utah statehood bargain, and a century of legal normalcy were all built on it. Internally, temple practice, the sealing order, and the authority structure administering them are organized around monogamy-with-doctrine-intact; removing the arrangement forces either resumed open practice (renewed state collision) or explicit renunciation (doctrinal rupture). Either way the world rearranges.
% FOUNDING_PROBLEM: Existential federal pressure: the Edmunds Act and Edmunds-Tucker Act criminalized the practice, imprisoned leaders, dissolved the church's legal incorporation, and threatened confiscation of its temples and property, while Utah statehood was conditioned on abandonment of plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: The founding pressure itself is corroborated from outside the benefiting parties: federal statutes, Supreme Court litigation (Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States, 1890), and the congressional record all attest it. Its death is equally corroborated: no enforcement action against the church on this ground in over a century, statehood achieved in 1896, and secular historiography treating the pressure as a closed nineteenth-century episode. No source outside the benefiting parties attests that the suspension still answers a live necessity.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at interval end) rises across the series because the burden converts in kind: in 1890 it reads as temporary sacrifice inside a live crisis; by 1930, with the pressure gone and no restoration, the same bar reads as permanent forfeiture of an ordinance the doctrine still calls exaltation-critical. Suppression (scalar 0.55) is authored as a raw structural property and is deliberately unscaled by power or scope; the suppression_requirement series tracks enforcement-capacity change specifically — lenient at first (clandestine and foreign marriages were widely tolerated through the 1890s), hardening sharply around the 1904 Second Manifesto and the Reed Smoot hearings, then easing as compliance normalized. Theater (0.42 at end) climbs as the arrangement's operative justification shifts from crisis management to canonical reassurance: retaining D&C 132 in the canon while practice is impossible performs continuity for the membership. One shared time grid (1890, 1896, 1904, 1910, 1920, 1930) carries all three series; every metric is authored at every point, so no end-state value leaks backward into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is inspired preservation — the coordination function dominates and the costs are divinely sanctioned. From the devout-believer seat — identity_locked, with a stake extending past death — the same arrangement operates as a heavy, open-ended denial. The federal seat experienced it as completed policy, with exit so mobile that its beneficiary position carries little ongoing weight. The excluded dissenters' seat marks the boundary: their refusal is the live counterfactual against which the reading's legitimacy claim is tested. Coalition note: the dissenters attempted exactly the coalition the powerless sometimes find — networks, foreign colonies, an alternative authority line — and the arrangement's enforcement absorbed them; that absorption is part of what the suppression series records.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the church corporation sits nearest the beneficiary pole (it collects the arrangement's principal continuities and demonstrated arbitrage-grade legal mobility); the federal government likewise near the beneficiary pole, its position further lightened by mobile exit; the membership sits mid-low (genuine relief, diffuse quiet costs via its secondary payer role). Victim declarations drive high d: devout believers sit nearest the target pole — identity_locked exit means no arbitrage softens their position; existing plural families are trapped and similarly near-full targets. Receipt: the arrangement's gains demonstrably accrue to the church corporation, which is why gain_flow names that seat rather than reporting diffuse. No directionality overrides are authored: the derivation from roles plus exit options already separates the seats correctly, and the override surface keys on power atoms, which would smear distinctions this story draws within one power level (three institutional seats with different directionalities are separated by role and exit, not by power).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem died early — statehood in 1896 and the lapse of enforcement removed the pressure the suspension answered — yet the arrangement persists and the reading still describes it as temporary. That is the classic mandatrophy shape, and the R5 mismatch (status dead x world rearranges) flags it: the suspension has outlived its mandate without either lifting (restoration) or converting into honest permanence (renunciation). Fixing is prohibitive for the only actor who could fix it: restoration reopens the state collision and shatters a century of settled identity; renunciation ruptures the canon and dissolves this very reading. The classification keeps both halves visible: the genuine coordination function (community survival under existential pressure) prevents mislabeling the whole as pure extraction, while the rising theater ratio and the dead founding problem prevent mistaking persistence for continued necessity. Mandatrophy is not resolved; the arrangement hangs in unresolved suspension, which is precisely the state this reading predicts will end in restoration — a prediction now generations overdue, tracked in the restoration_expectancy_status omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does this classification hold only under the temporal_accommodation_reading, or is it robust across the kernel''s readings?',
    'Compile the sibling stories (immutable_commandment_reading, prophetic_override_reading) and compare computed types, victim sets, and epsilon across readings of the same kernel.',
    'Under the immutable reading the victim set expands to every compliant member (their own suspension becomes the injury) and epsilon rises sharply; under the override reading authority relocates to the living prophet and the enforcement picture changes. Cross-reading divergence is expected and is the measurement, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested kernel; siblings are other constraints, not errors.').

omega_variable(
    restoration_expectancy_status,
    'Is the reading''s expectancy — restoration when political constraints lift — still live, or has it silently become permanent dormancy?',
    'Track official statements, curriculum treatment of D&C 132, and member belief surveys across decades; test whether any institutional actor still asserts restoration as expectation rather than abstraction.',
    'If expectancy is dead, the retained doctrine is inertial and the story drifts toward piton dynamics (theatrical maintenance of a dormant principle); if live, the transition promise retains force and the founding-problem mismatch overstates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_expectancy_status, empirical, 'Whether the reading''s predictive content (conditional restoration) survives its own deadline having passed.').

omega_variable(
    manifesto_sincerity,
    'Was the Manifesto a genuine revelatory accommodation, or a strategic capitulation retroactively dressed as revelation?',
    'Archival work: Woodruff''s diaries and contemporaneous apostolic minutes against the public framing; comparison of internal deliberation chronology with the published account.',
    'If strategic, the theater ratio understates performance and the reading''s legitimacy claim weakens toward cover-for-capitulation, pulling the computed profile toward snare-flavored classifications; if sincere, the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_sincerity, empirical, 'Sincerity of the suspension''s revelatory framing versus its strategic timing.').

omega_variable(
    suppression_attribution,
    'Is the measured suppression primarily external (federal coercion the church accommodated) or internal (ecclesiastical discipline against practitioners and dissenters)?',
    'Decompose enforcement actions by origin: prosecutions and property seizures (external) versus temple exclusions, disciplines, and excommunications (internal), weighted by behavioral effect.',
    'If predominantly internal, the arrangement''s coercive force is self-generated and the extraction profile hardens; if predominantly external, the leadership seat reads closer to fellow accommodator than enforcer, softening its distance from the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_attribution, empirical, 'Internal versus external sourcing of the arrangement''s coercive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement_basis(eter_tr_t1890, observed).
narrative_ontology:measurement(eter_tr_t1896, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1896, 0.18).
narrative_ontology:measurement_basis(eter_tr_t1896, observed).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.24).
narrative_ontology:measurement_basis(eter_tr_t1904, observed).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1910, 0.3).
narrative_ontology:measurement_basis(eter_tr_t1910, observed).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.36).
narrative_ontology:measurement_basis(eter_tr_t1920, observed).
narrative_ontology:measurement(eter_tr_t1930, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1930, 0.42).
narrative_ontology:measurement_basis(eter_tr_t1930, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.38).
narrative_ontology:measurement_basis(eter_be_t1890, observed).
narrative_ontology:measurement(eter_be_t1896, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1896, 0.44).
narrative_ontology:measurement_basis(eter_be_t1896, observed).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.5).
narrative_ontology:measurement_basis(eter_be_t1904, observed).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1910, 0.54).
narrative_ontology:measurement_basis(eter_be_t1910, observed).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement_basis(eter_be_t1920, observed).
narrative_ontology:measurement(eter_be_t1930, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement_basis(eter_be_t1930, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.3).
narrative_ontology:measurement_basis(eter_su_t1890, observed).
narrative_ontology:measurement(eter_su_t1896, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1896, 0.38).
narrative_ontology:measurement_basis(eter_su_t1896, observed).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.55).
narrative_ontology:measurement_basis(eter_su_t1904, observed).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1910, 0.6).
narrative_ontology:measurement_basis(eter_su_t1910, observed).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(eter_su_t1920, observed).
narrative_ontology:measurement(eter_su_t1930, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement_basis(eter_su_t1930, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto's meaning' decomposes into three structurally distinct readings of the eternal_marriage_covenant kernel, each with its own epsilon, victim set, and classification. This story is the temporal_accommodation_reading (suspension legitimate, doctrine dormant, restoration expected). The immutable_commandment_reading (upstream in doctrinal confidence: D&C 132 as settled eternal law) feeds this reading's premise that the principle remains valid; the prophetic_override_reading (downstream: authority to supersede) is reinforced by each circumstantial suspension this reading performs. All three files link via network.affects_constraints; epsilon differs across the family because the victim set and enforcement locus differ, not because one constraint is measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
