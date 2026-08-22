% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Retention Regime over Indigenous Cultural Artifacts (Indigenous Stewardship Reading)
 *   domain: legal/post-colonial/cultural
 *
 * SUMMARY:
 *   The standing arrangement under contest is the retention regime: hundreds
 *   of thousands of indigenous sacred and communal objects held by
 *   encyclopedic museums and by successor states that inherited colonial
 *   acquisitions, governed by anti-deaccessioning norms,
 *   immunity-from-seizure statutes, limitation defenses, and evidentiary
 *   burdens placed on claimant communities. This story instantiates the
 *   indigenous_stewardship_reading of the cultural_property_legal_corpus
 *   kernel: custodial authority vests in communities maintaining cultural
 *   continuity, and neither museums nor successor states hold legitimate
 *   title. Per the epsilon-referent rule, extractiveness is authored for THAT
 *   standing arrangement as this reading assesses it, not for the
 *   community-custody arrangement this reading endorses; the endorsed
 *   alternative would trivially score near zero and is not the referent. The
 *   claim/metric independence rule is respected: the claimed type is stated
 *   from this reading's structural seat, and the metrics describe the
 *   arrangement's observed operation. The sibling readings are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - indigenous_origin_communities: Primary target (organized/identity_locked) — bear loss of ceremonial continuity and control; cannot exit the relationship to the objects
 *   - - ceremonial_custodians: Excluded knowledge-holders (powerless/trapped) — consent never sought, no governance seat
 *   - - encyclopedic_museums: Agenda-setter and principal collector (institutional/arbitrage) — administer holdings, set claim procedures, monetize custody
 *   - - colonial_successor_states: Dual-positioned beneficiary-administrator (institutional/constrained) — run national museums and export controls over others' patrimony
 *   - - heritage_tourism_sector: Secondary beneficiary (organized/mobile) — sells access premised on objects staying put
 *   - - collection_dependent_researchers: Secondary beneficiary (moderate/constrained) — careers and datasets built on stable access
 *   - - cultural_property_legal_scholars: Analytical observer (analytical/analytical) — maps doctrine without holding or owing objects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.85).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.72).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Retention Regime over Indigenous Cultural Artifacts (Indigenous Stewardship Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "legal/post-colonial/cultural").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, 'b8517058-ec37-4fd9-9e4c-94143874b464').
narrative_ontology:cs_kernel_codification('b8517058-ec37-4fd9-9e4c-94143874b464', distributed).
narrative_ontology:cs_authority_grounding('b8517058-ec37-4fd9-9e4c-94143874b464', distributed).
narrative_ontology:cs_reading_relation('b8517058-ec37-4fd9-9e4c-94143874b464', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8517058-ec37-4fd9-9e4c-94143874b464', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('b8517058-ec37-4fd9-9e4c-94143874b464', foundational, continuity_confers_custodial_authority).
narrative_ontology:cs_axiom_status(continuity_confers_custodial_authority, holdable).
narrative_ontology:cs_axiom_grounding('b8517058-ec37-4fd9-9e4c-94143874b464', continuity_confers_custodial_authority, deontological).
narrative_ontology:cs_axiom('b8517058-ec37-4fd9-9e4c-94143874b464', foundational, colonial_acquisition_void_of_title).
narrative_ontology:cs_axiom_status(colonial_acquisition_void_of_title, holdable).
narrative_ontology:cs_axiom_grounding('b8517058-ec37-4fd9-9e4c-94143874b464', colonial_acquisition_void_of_title, deontological).
narrative_ontology:cs_reference_frame('b8517058-ec37-4fd9-9e4c-94143874b464', continuity_community_custody).
narrative_ontology:cs_drift_state('b8517058-ec37-4fd9-9e4c-94143874b464', contemporary_retention_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8517058-ec37-4fd9-9e4c-94143874b464', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, heritage_tourism_sector).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, collection_dependent_researchers).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_origin_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, ceremonial_custodians).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_authority_principle).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, free_prior_informed_consent_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold ceremonial and historical relationships to objects now stored and displayed far away. Some objects anchor ceremonies that cannot proceed without them; others carry kin histories and obligations. Representatives file repatriation requests, negotiate loans or co-stewardship, and build local repositories where returns arrive. Leaving the situation is not a meaningful option: the relationship to the objects persists wherever the objects sit.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_origin_communities, payer,
    organized, generational, identity_locked, global).

% Knowledge-holders responsible for handling protocols attached to specific objects. Their consent was not sought at acquisition and they hold no seat in holding-institution governance; they learn of exhibitions, image sales, or handling decisions when photographs circulate. Their leverage is testimony, protest, and the few consultation channels that exist.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, ceremonial_custodians, excluded,
    powerless, generational, trapped, local).

% Administer the holdings: set conservation standards, exhibition schedules, image licensing, and loan policy, and decide which repatriation claims to entertain and which to litigate. Income flows from admissions, touring exhibitions, licensing, and donor prestige tied to signature collections. Under pressure they can adjust through loans, digital surrogates, and partnership language without surrendering custody.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, beneficiary).

% Operate national museums and export-control regimes over materials acquired from peoples they do not descend from or represent under this reading's criterion. They draw tourism revenue, school curricula, and diplomatic standing from the holdings, and they resist claims arriving both from abroad and from internal minority communities.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter).

% Sells access to the collections through blockbusters, city marketing, and licensed merchandise. Its product line depends on objects staying put; it lobbies quietly against deaccession and funds galleries that reinforce the holding institutions' standing.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, heritage_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Careers, datasets, and training pipelines are built on stable access to the collections. Many support collaboration and some champion return; exit for the profession as currently constituted would mean rebuilding fields around community-held archives.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, collection_dependent_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Map the doctrinal terrain: provenance gaps, immunity statutes, limitation defenses, and the growing soft-law layer. They publish assessments, advise commissions, and observe without holding or owing any of the objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates conservation expertise, climate-controlled storage, cataloguing, and protection from illicit trade in a small number of institutions, giving scholars and publics reliable access to material from many cultures in one place.
% TRANSFER_FUNCTION: Moves custody, display revenue, image-licensing income, and scholarly and national-prestige capital from indigenous origin communities to holding museums and successor states, and moves the burden of proving entitlement back onto claimant communities.
% ABSENT_VOICES: Ceremonial custodians and origin-community elders were absent when acquisitions occurred and remain marginal in holding-institution governance; diaspora members who cannot travel to the holding institutions; and the deceased whose funerary objects are on display. Their objections surface only through protest and the limited consultation channels that exist.
% DISAPPEARANCE_RATIONALE: If the retention regime vanished overnight, encyclopedic museums would lose core collections that anchor their identities and revenues, successor-state tourism and curricula would reorganize, scholarship would rebuild around community-held archives, and tens of thousands of objects would move to community custody — the heritage economy would rearrange around the new custodial map rather than continue as before.
% FOUNDING_PROBLEM: Colonial-era removal was justified as rescue: material deemed at risk from neglect, looting, war, and local instability was concentrated in institutions that could preserve it and make it available for study.
% FOUNDING_PROBLEM_CORROBORATION: Holding institutions and their governments attest the preservation rationale as live, citing conflict-zone looting and climate threats in museum reports and ICOM statements. Corroboration from outside the benefiting parties for the shifted-function reading includes the Sarr-Savoy report commissioned by the French presidency, the General Assembly's adoption of UNDRIP Articles 11-12, and the congressional findings behind NAGPRA — each attesting that retention now rests on inertia and legal defense rather than a live rescue need for the contested stock.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because under this reading every holder lacks legitimate title, custody converts directly into admission, licensing, touring, and prestige income, and the burden of proving entitlement sits on the dispossessed. Suppression (0.72) is structural rather than violent: immunity statutes, limitation periods, deaccession prohibitions, and resource-asymmetric litigation close the legal exits. Theater (0.35) captures the growing share of institutional activity that acknowledges colonial acquisition in wall texts and reports while transferring no authority — statements, token loans, and partnership language. Accessibility_collapse is moderate (0.45): the alternative is visible and increasingly practiced (NAGPRA-class regimes, Benin returns), so understanding the reading does not collapse alternatives the way a natural law would. Resistance is high (0.70): sustained repatriation movements, UNDRIP, national commissions, and litigation meet the regime continuously. The temporal series run on one shared seven-point grid; suppression_requirement is authored because enforcement capacity is the traced dynamic — legal fortification through the middle decades, then partial substitution by soft-power accommodation, visible as the plateau and slight decline after t=40 while theater keeps rising.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the museum seat the arrangement is stewardship it built and funds: conservation, cataloguing, and access look like services, computing rope-like. From the community seat the same services are performed on taken property and the offered good (preservation apart from the community) is not the good the community is owed, computing snare-like. From the successor-state seat the picture splits: strong title over its own heritage, none over internal minorities' material. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedic_museums sit nearest the beneficiary pole (d near 0.05): full custody, arbitrage-grade adaptive exits. Colonial_successor_states derive low d (near 0.15) as declared beneficiaries, tempered by exposure to internal-minority claims. Heritage_tourism and collection_dependent_researchers are indirect beneficiaries (d near 0.2-0.25) via the beneficiary declarations. Indigenous_origin_communities derive near-full-target d (near 0.95): declared victims, identity_locked exit, generational stakes. Ceremonial_custodians sit at the extreme (d near 1.0): trapped, excluded, and bearing the protocol-specific harms. No directionality overrides are needed; the beneficiary/victim declarations plus exit atoms produce the correct spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rescue of at-risk material and its availability to scholarship) is contested rather than dead: for material in active conflict zones the rescue rationale retains force even in this reading's lights, while for sacred material the rationale is void because the separation itself is the harm. Because status is contested rather than dead, the dead-found-problem-times-world-rearranges mismatch does not fire, and mandatrophy_resolved is correctly left undeclared — the arrangement's mandate is disputed, not outlived. The classification discipline cuts both ways: crediting conservation as a coordination benefit would launder the authority defect this reading identifies, while denying all function would erase the real conservation record that shapes which remedies are feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (indigenous_stewardship_reading) of the cultural_property_legal_corpus kernel; would instantiating a sibling reading change the beneficiary/victim structure and epsilon outright?',
    'Author the sibling stories (universal_heritage_reading, sovereign_repatriation_reading) as separate files and compare computed classifications across the family; a binding multilateral instrument allocating custodial authority would resolve the contest politically.',
    'Under the universal_heritage_reading the same retention arrangement computes near the coordination-cost floor with museums as legitimate stewards; under the sovereign_repatriation_reading successor states drop out of the extractor set and only foreign-held collections carry high extraction. The epsilon authored here (0.85) is the maximum of the family because both holder classes lack legitimate title under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    continuity_determination_ambiguity,
    'Who counts as a community maintaining cultural continuity sufficient to hold custodial authority under this reading, given missionization, displacement, and interrupted transmission?',
    'Genealogical and linguistic evidence, community self-identification combined with external expert testimony, and the developing jurisprudence of domestic regimes that already apply continuity tests.',
    'A strict continuity test narrows the beneficiary set and partially rehabilitates successor-state claims over orphaned collections; a descent-plus-self-identification test widens community authority and pushes epsilon higher for remaining holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_determination_ambiguity, conceptual, 'The reading''s own authority criterion (cultural continuity) has an indeterminate boundary.').

omega_variable(
    sacred_communal_scope_boundary,
    'Does one epsilon cover both sacred objects (whose separation from the community is itself the harm) and communal objects (where negotiated co-stewardship is a live community demand), or do these decompose into two constraints?',
    'Classify the contested stock by object class and test whether measured extraction and remedy preferences diverge systematically; if they do, split into separate stories linked by network edges.',
    'If sacred objects dominate the contested stock, effective extraction trends toward the top of the range and negotiated remedies under-deliver; if communal objects dominate, partnership structures lower effective extraction without transferring full authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_communal_scope_boundary, empirical, 'Possible internal decomposition of the reading''s object domain along the sacred/communal line.').

omega_variable(
    deference_internalization_mix,
    'Where origin communities accommodate to museum custody through loans and digital access, is that settled preference or adaptation to unavailable alternatives?',
    'Compare communities with realistic repatriation pathways against those without: if accommodation rates converge once pathways open, prior accommodation reflected barrier adaptation rather than preference.',
    'If much apparent acquiescence is barrier-adaptation, the arrangement''s suppression is understated by consent-based framings and effective extraction is higher than participation statistics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deference_internalization_mix, empirical, 'Distinguishing genuine preference for institutional custody from adaptation to blocked exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indigenous_stewardship_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t0, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t10, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t20, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t30, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t40, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t50, observed).
narrative_ontology:measurement(indigenous_stewardship_tr_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(indigenous_stewardship_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(indigenous_stewardship_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t0, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t10, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t20, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t30, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t40, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t50, observed).
narrative_ontology:measurement(indigenous_stewardship_be_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement_basis(indigenous_stewardship_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(indigenous_stewardship_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t0, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t10, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t20, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t30, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t40, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 50, 0.73).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t50, observed).
narrative_ontology:measurement(indigenous_stewardship_su_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(indigenous_stewardship_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'cultural property law' decomposes into three readings of one kernel with distinct epsilon values over the same standing arrangement. universal_heritage_reading is the upstream, established frame (low epsilon, museums as legitimate stewards); sovereign_repatriation_reading is intermediate (successor states legitimate, foreign museums extractive); this indigenous_stewardship_reading is downstream and assigns the highest epsilon because both holder classes lack legitimate title. Each member links the others via affects_constraints; the upstream frame is routinely cited as evidence by the downstream frames, which is why contamination propagates from universal_heritage toward the restitution readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
