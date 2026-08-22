% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Reification via Colonial Census Administration
 *   domain: social/political/administrative
 *
 * SUMMARY:
 *   Under colonial administration in India, jati categories—which had been
 *   locally negotiated, occupationally fluid, and subject to continuous
 *   reclassification—were reified into fixed, hereditary, written categories
 *   and embedded in census enumeration, revenue assessment, and legal
 *   jurisdiction. This reading instantiates the constraint that results: the
 *   colonial administrative apparatus (agenda-setter) benefits from
 *   predictable governance; upper-jati groups benefit from legal entrenchment
 *   of their status; lower-jati communities and practitioners of fluid
 *   occupational identities bear the cost of foreclosed mobility and
 *   identity-locking to an administratively assigned category. The constraint
 *   is claimed as tangled_rope because it achieves genuine administrative
 *   coordination while imposing asymmetric extraction through freezing
 *   previously fluid boundaries.
 *
 * KEY AGENTS:
 *   - colonial_administrative_apparatus: Writes the census, enforces jati categories through revenue and legal codes, benefits from predictable taxation and conscription
 *   - brahminical_hierarchy_beneficiaries: Upper-jati landholders and merchants whose claimed status is now backed by colonial legal force, gaining entrenchment and protection
 *   - lower_jati_communities: Powerless, identity-locked by census assignment; lose occupational mobility and acquire legal disabilities tied to recorded jati
 *   - intercaste_practitioners: Moderate power, constrained exit; occupational groups that crossed jati lines face administrative pressure to declare a single fixed category
 *   - orthodox_textual_interpreters: Brahminical authorities whose varna frameworks gain new enforcement machinery through colonial codification
 *   - localized_negotiating_communities: Village assemblies and jati councils excluded from the new system; previously negotiated boundaries now overridden by external codes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.68).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.72).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Reification via Colonial Census Administration").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social/political/administrative").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '27881642-9cab-4f39-b4f0-ebcd560b4b37').
narrative_ontology:cs_kernel_codification('27881642-9cab-4f39-b4f0-ebcd560b4b37', formalized).
narrative_ontology:cs_authority_grounding('27881642-9cab-4f39-b4f0-ebcd560b4b37', extraction).
narrative_ontology:cs_interpretation_layer_present('27881642-9cab-4f39-b4f0-ebcd560b4b37').
narrative_ontology:cs_reading_relation('27881642-9cab-4f39-b4f0-ebcd560b4b37', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('27881642-9cab-4f39-b4f0-ebcd560b4b37', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('27881642-9cab-4f39-b4f0-ebcd560b4b37', foundational, jati_categories_administratively_fixed).
narrative_ontology:cs_axiom_status(jati_categories_administratively_fixed, holdable).
narrative_ontology:cs_axiom_grounding('27881642-9cab-4f39-b4f0-ebcd560b4b37', jati_categories_administratively_fixed, conventional).
narrative_ontology:cs_axiom('27881642-9cab-4f39-b4f0-ebcd560b4b37', foundational, written_census_overrides_local_negotiation).
narrative_ontology:cs_axiom_status(written_census_overrides_local_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('27881642-9cab-4f39-b4f0-ebcd560b4b37', written_census_overrides_local_negotiation, deontological).
narrative_ontology:cs_reference_frame('27881642-9cab-4f39-b4f0-ebcd560b4b37', administrative_rationality_framework).
narrative_ontology:cs_drift_state('27881642-9cab-4f39-b4f0-ebcd560b4b37', post_colonial_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27881642-9cab-4f39-b4f0-ebcd560b4b37', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, brahminical_hierarchy_beneficiaries).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, lower_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, intercaste_practitioners).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_occupational_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, orthodox_textual_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses census categories and administrative codes to codify jati boundaries for taxation, conscription, and legal jurisdiction. Freezes previously fluid categories into fixed, hereditary slots to simplify governance and reduce administrative uncertainty. Enforces the reified boundaries through legal recognition and revenue collection tied to jati status.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Upper-jati landholders and merchants whose social standing and economic claims are stabilized and legally protected by the reified system. The colonial apparatus enforces what had been negotiated or contested local hierarchies as fixed law, crystallizing upper-jati advantage into administrative precedent.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, brahminical_hierarchy_beneficiaries, beneficiary,
    powerful, generational, mobile, national).

% Locked into hereditary occupational categories by census enumeration and administrative coding. Their jati is recorded as a legal attribute, barring exit through occupational mobility, geographic relocation, or ritual reclassification—options available before colonial reification. They bear increased taxation burden indexed to jati status and face legal disabilities (exclusions from land ownership, water access, temple entry) that the apparatus codifies.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, lower_jati_communities, payer,
    powerless, generational, identity_locked, national).

% Occupational groups that previously crossed jati lines—mixed-caste villages, occupational clusters without rigid jati boundaries, practitioners who combined ritual and commercial functions—face administrative pressure to declare a single fixed jati for census and revenue purposes. The fluidity that sustained their livelihoods is replaced by categorical rigidity.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, intercaste_practitioners, payer,
    moderate, biographical, constrained, local).

% Brahminical authorities and textual scholars whose varna-based frameworks are now backed by colonial legal force. Their interpretive authority gains new enforcement muscle; deviations from scriptural classification are no longer merely ritual transgressions but legal violations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, orthodox_textual_interpreters, beneficiary,
    powerful, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, orthodox_textual_interpreters, observer).

% Village assemblies and local jati councils that previously negotiated jati boundaries, status claims, and ritual precedence through cyclical dispute-resolution and consensus-forming are bypassed. The census and administrative code impose external categories that override local deliberation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, localized_negotiating_communities, excluded,
    organized, biographical, trapped, local).

% Census commissioners and administrative theorists who treat jati reification as a necessary rationalization of Indian society for predictable governance. They view the constraint as imposing scientific order on apparent chaos, unaware that the 'chaos' sustained occupational and social dynamism.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_metropolitan_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces administrative uncertainty for tax collection, military conscription, and legal jurisdiction by converting locally-negotiated, fluid jati boundaries into fixed, hereditary, written categories that the colonial apparatus can enumerate and apply uniformly.
% TRANSFER_FUNCTION: Moves occupational autonomy and social mobility (previously available through local reclassification, ritual transition, occupational relocation, geographic movement) from lower-jati and intercaste communities to the colonial administrative apparatus and upper-jati beneficiaries. The apparatus gains tax base predictability; upper-jati groups gain legal protection and entrenchment; lower-jati communities acquire legal disabilities indexed to recorded jati and lose escape routes.
% ABSENT_VOICES: Village jati councils, occupational guilds, and localized deliberating bodies that previously negotiated jati boundaries and status claims are structurally excluded from census enumeration and administrative codification. They would attest that the constraint eliminates the negotiation pathways through which mobility and status reclassification historically occurred.
% DISAPPEARANCE_RATIONALE: If the reification apparatus vanished, jati categories would re-fluidify within one to two generations. Communities would resume occupational mobility across jati lines, local councils would reopen jati boundary negotiations, and the hereditary census slot would cease to be an administrative or legal fact. Revenue and conscription systems would revert to locally-negotiated assessments.
% FOUNDING_PROBLEM: The colonial administration encountered thousands of locally-distinct and overlapping jati designations that varied by village, ritual context, and occupational season. Census enumeration, revenue assessment, and military conscription required a single, uniform, written classification of the population for predictable governance.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrators attested to the problem in census reports and administrative correspondence (describing Indian society as chaotic and lacking rational order). Historians and anthropologists outside the benefiting parties document that pre-colonial jati categories were negotiated locally and occupationally fluid, and that the 'chaos' described was actually the normal operation of a system accommodating mobility—not a pathology requiring cure.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 (colonial inception, weak enforcement apparatus) to 0.68 (established reified system with legal penalties for non-compliance and revenue extraction indexed to jati status), plateauing at the endpoint. The measurement series captures the acceleration of reification as colonial administrative capacity grew and legal codes crystallized the categories. Theater_ratio remains moderate (0.41) because the constraint carries a genuine coordination function (simplifying governance) alongside its extraction—the security review is real, not purely theatrical, but defensive machinery grows over time to prevent boundary-crossing escapes. Suppression tracks similarly, rising from 0.35 to 0.72 as enforcement mechanisms harden (legal codes, police enforcement of occupational restrictions, temple-entry enforcement, land-access rules tied to recorded jati). Accessibility_collapse peaks at 0.72 for individuals in lower jati communities by endpoint because census assignment forecloses the local-negotiation and ritual-reclassification pathways that had previously existed. The leveled coercion grid shows that suppression and accessibility collapse hit hardest at the individual and class levels (lower-jati people and occupationally-fluid groups), while resistance weakens most at the organizational and structural levels as local councils are bypassed and the administrative framework solidifies.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administrative seat, the constraint is coordination—imposing rational order on incomprehensible local variation to enable predictable governance and revenue collection. From the lower-jati and intercaste-practitioner seats, the same structure is coercive extraction: loss of occupational mobility, legal disabilities, and foreclosure of the local deliberation that had previously allowed status renegotiation and upward movement. The engine computes per-seat classifications from the structural data: the beneficiary seats (upper-jati, brahminical, administrative) experience low directionality (benefiting from entrenchment); the payer seats (lower-jati identity-locked communities) experience high directionality (targeted by exclusions and disabilities). Directionality overrides are not necessary because the power and exit atoms (powerless + identity_locked for lower-jati communities; powerful + mobile for upper-jati) already encode the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administrative apparatus: institutional power, arbitrage exit (can redesign the system or exit the colony)—low directionality (~0.10), benefits from the constraint. Brahminical hierarchy beneficiaries: powerful but dependent on the system's preservation—moderate directionality (~0.25), net beneficiary. Lower-jati communities: powerless, identity-locked to recorded jati (no biological reclassification, no ritual exit from census category, no geographic escape without jati-based legal disability following them)—high directionality (~0.85), heavily targeted. Intercaste practitioners: moderate power, constrained exit (can move geographically but carry their jati assignment with them)—high-moderate directionality (~0.68), targeted by categorical rigidity. The identity-locking is the key structural feature: prior to census reification, a person could change occupation, move to a different village and claim a different jati, undergo ritual reclassification, or practice across jati boundaries; the constraint's reification converts these local, negotiable designations into legal facts that follow the person.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative chaos and unpredictability) is contested by endpoint. Lower-jati testimony and historians outside the benefiting parties argue that the pre-colonial system did not produce 'chaos' but rather a working system of continuous negotiation that accommodated occupational and social dynamism. The constraint's founding justification (need for predictable governance) persists, but the problem it names is increasingly exposed as a colonial reading that mistook negotiation for disorder. This is a classic mandatrophy case: the founding problem may have driven inception but became contested as beneficiaries learned to manage the reified system and as payers learned that mobility was the cost of 'rationalization.' The theater_ratio rise (from 0.08 to 0.41) tracks the constraint's shift from purely extractive enforcement toward defensive performance—maintaining the fiction that categories are natural or scriptural (invoking varna tradition) while actually defending the administrative apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locking_mechanism_depth,
    'How complete is the identity-locking mechanism binding lower-jati communities to their census-assigned category? Can exits occur through religious conversion, ritual practices, geographic relocation, or occupational retraining, or are these exits foreclosed by the legal system?',
    'Case law analysis of jati-change petitions, revenue department records of exit attempts, ethnographic documentation of how often communities successfully exit their recorded jati, longitudinal mobility data across census cycles.',
    'If exits are rare (true identity-locking), the effective extraction is higher and the suppression is more foundational. If exits are frequent despite administrative pressure (partial locking), the constraint''s hold is more performative than structural, pushing toward piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_mechanism_depth, empirical, 'The depth of identity-locking to assigned jati categories in practice vs. in law.').

omega_variable(
    coordination_necessity_question,
    'Would colonial governance and revenue collection have been impossible without jati reification? Or could the apparatus have achieved similar coordination outcomes by allowing local negotiation and imposing uniform reporting standards without fixing categories?',
    'Comparative analysis of colonial administrative systems in non-jati societies; historical counterfactual analysis of what would have occurred if the colonial apparatus imposed only reporting standards without categorical reification; ethnographic study of parallel coordination mechanisms in communities that resisted census reification.',
    'If reification was strictly necessary for coordination, the constraint splits more cleanly into coordination + extraction. If coordination could have been achieved without freezing categories, the extraction is more primary and the coordination is a post-hoc cover story, tilting toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_question, conceptual, 'Whether jati reification was structurally necessary for administrative coordination or merely convenient.').

omega_variable(
    reading_transition_mechanism,
    'How is this reading (colonial_census_reading) maintained in post-colonial India? Does the apparatus persist because of inertia (piton), because new state actors benefit from it (snare or tangled_rope), or because communities have internalized the categories as natural (internalized suppression)?',
    'Post-colonial administrative history; analysis of which stakeholders lobby to preserve census jati enumeration; ethnographic study of how communities describe their jati (as assigned vs. chosen); measurement of enforcement intensity post-independence.',
    'If persistence is purely inertial (piton), the constraint becomes vulnerable to removal. If new beneficiaries emerged (snare or tangled_rope), the constraint gains new structural support. If communities internalized the categories, suppression becomes internalized and harder to disrupt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_transition_mechanism, empirical, 'The mechanism sustaining jati reification post-colonialism and the reading''s continuation into new institutional contexts.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the colonial_census_reading logically foreclose the localized_practice_reading, or do they coexist as competing framings held by different parties?',
    'Legal analysis of whether colonial codes explicitly reject localized negotiation, or merely impose a parallel centralized system. Ethnographic documentation of whether communities continue local jati negotiation alongside census recognition, or whether the census eliminates the local deliberative space entirely.',
    'Foreclosure would indicate that one reading''s core premise directly contradicts the other''s. Coexistence would indicate that both framings remain live options in different spheres (legal vs. social, state vs. community), suggesting the kernel has not been resolved but merely stratified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the readings coexist as stratified framings or whether one forecloses the other within the same institutional space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(jati_tr_t0, projected).
narrative_ontology:measurement(jati_tr_t8, jati_practice_norm__colonial_census_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(jati_tr_t8, observed).
narrative_ontology:measurement(jati_tr_t16, jati_practice_norm__colonial_census_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement_basis(jati_tr_t16, observed).
narrative_ontology:measurement(jati_tr_t24, jati_practice_norm__colonial_census_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(jati_tr_t24, observed).
narrative_ontology:measurement(jati_tr_t32, jati_practice_norm__colonial_census_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement_basis(jati_tr_t32, observed).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__colonial_census_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(jati_tr_t40, observed).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__colonial_census_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(jati_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jati_be_t0, projected).
narrative_ontology:measurement(jati_be_t8, jati_practice_norm__colonial_census_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(jati_be_t8, observed).
narrative_ontology:measurement(jati_be_t16, jati_practice_norm__colonial_census_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(jati_be_t16, observed).
narrative_ontology:measurement(jati_be_t24, jati_practice_norm__colonial_census_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(jati_be_t24, observed).
narrative_ontology:measurement(jati_be_t32, jati_practice_norm__colonial_census_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(jati_be_t32, observed).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__colonial_census_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(jati_be_t40, observed).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__colonial_census_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(jati_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(jati_su_t0, projected).
narrative_ontology:measurement(jati_su_t8, jati_practice_norm__colonial_census_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(jati_su_t8, observed).
narrative_ontology:measurement(jati_su_t16, jati_practice_norm__colonial_census_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(jati_su_t16, observed).
narrative_ontology:measurement(jati_su_t24, jati_practice_norm__colonial_census_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(jati_su_t24, observed).
narrative_ontology:measurement(jati_su_t32, jati_practice_norm__colonial_census_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(jati_su_t32, observed).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__colonial_census_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(jati_su_t40, observed).
narrative_ontology:measurement(jati_su_t50, jati_practice_norm__colonial_census_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(jati_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(jati_grid_01, jati_practice_norm__colonial_census_reading, accessibility_collapse(class), 0, 0.15).
narrative_ontology:measurement(jati_grid_02, jati_practice_norm__colonial_census_reading, accessibility_collapse(class), 50, 0.62).
narrative_ontology:measurement(jati_grid_03, jati_practice_norm__colonial_census_reading, accessibility_collapse(individual), 0, 0.25).
narrative_ontology:measurement(jati_grid_04, jati_practice_norm__colonial_census_reading, accessibility_collapse(individual), 50, 0.72).
narrative_ontology:measurement(jati_grid_05, jati_practice_norm__colonial_census_reading, accessibility_collapse(organizational), 0, 0.18).
narrative_ontology:measurement(jati_grid_06, jati_practice_norm__colonial_census_reading, accessibility_collapse(organizational), 50, 0.68).
narrative_ontology:measurement(jati_grid_07, jati_practice_norm__colonial_census_reading, accessibility_collapse(structural), 0, 0.22).
narrative_ontology:measurement(jati_grid_08, jati_practice_norm__colonial_census_reading, accessibility_collapse(structural), 50, 0.65).
narrative_ontology:measurement(jati_grid_09, jati_practice_norm__colonial_census_reading, resistance(class), 0, 0.6).
narrative_ontology:measurement(jati_grid_10, jati_practice_norm__colonial_census_reading, resistance(class), 50, 0.52).
narrative_ontology:measurement(jati_grid_11, jati_practice_norm__colonial_census_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(jati_grid_12, jati_practice_norm__colonial_census_reading, resistance(individual), 50, 0.42).
narrative_ontology:measurement(jati_grid_13, jati_practice_norm__colonial_census_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(jati_grid_14, jati_practice_norm__colonial_census_reading, resistance(organizational), 50, 0.38).
narrative_ontology:measurement(jati_grid_15, jati_practice_norm__colonial_census_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(jati_grid_16, jati_practice_norm__colonial_census_reading, resistance(structural), 50, 0.35).
narrative_ontology:measurement(jati_grid_17, jati_practice_norm__colonial_census_reading, stakes_inflation(class), 0, 0.25).
narrative_ontology:measurement(jati_grid_18, jati_practice_norm__colonial_census_reading, stakes_inflation(class), 50, 0.7).
narrative_ontology:measurement(jati_grid_19, jati_practice_norm__colonial_census_reading, stakes_inflation(individual), 0, 0.3).
narrative_ontology:measurement(jati_grid_20, jati_practice_norm__colonial_census_reading, stakes_inflation(individual), 50, 0.75).
narrative_ontology:measurement(jati_grid_21, jati_practice_norm__colonial_census_reading, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(jati_grid_22, jati_practice_norm__colonial_census_reading, stakes_inflation(organizational), 50, 0.68).
narrative_ontology:measurement(jati_grid_23, jati_practice_norm__colonial_census_reading, stakes_inflation(structural), 0, 0.32).
narrative_ontology:measurement(jati_grid_24, jati_practice_norm__colonial_census_reading, stakes_inflation(structural), 50, 0.72).
narrative_ontology:measurement(jati_grid_25, jati_practice_norm__colonial_census_reading, suppression(class), 0, 0.25).
narrative_ontology:measurement(jati_grid_26, jati_practice_norm__colonial_census_reading, suppression(class), 50, 0.75).
narrative_ontology:measurement(jati_grid_27, jati_practice_norm__colonial_census_reading, suppression(individual), 0, 0.22).
narrative_ontology:measurement(jati_grid_28, jati_practice_norm__colonial_census_reading, suppression(individual), 50, 0.68).
narrative_ontology:measurement(jati_grid_29, jati_practice_norm__colonial_census_reading, suppression(organizational), 0, 0.18).
narrative_ontology:measurement(jati_grid_30, jati_practice_norm__colonial_census_reading, suppression(organizational), 50, 0.72).
narrative_ontology:measurement(jati_grid_31, jati_practice_norm__colonial_census_reading, suppression(structural), 0, 0.4).
narrative_ontology:measurement(jati_grid_32, jati_practice_norm__colonial_census_reading, suppression(structural), 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel decomposes into three structurally distinct constraint stories representing three readings of the same contested kernel. Each reading instantiates different beneficiaries, different victims, and different ε values: (1) orthodox_textual_reading treats jati as fixed scriptural varna with low extraction (mountain or rope); (2) localized_practice_reading treats jati as fluid coordination norms with low-to-moderate extraction (rope); (3) colonial_census_reading (this one) treats jati as administratively reified categories with moderate-high extraction (tangled_rope). The readings coexist across different parties and institutional spheres—no single party holds only one reading. The colonial apparatus and benefiting upper-jati groups advance the census reading; communities and some scholars advance the localized-practice reading; orthodox interpreters advance the textual reading. This story links to its siblings because the colonial apparatus's victory in imposing the census reading was enabled by alignment with orthodox textual authority and against the localized-practice reading's claims about fluidity. The network edge direction runs: orthodox_textual_reading influences colonial_census_reading (textual authority is invoked to legitimize fixed categories); colonial_census_reading forecloses (or severely constrains) localized_practice_reading within the legal/administrative sphere (though local practice continues outside state purview, unresolved tension).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
