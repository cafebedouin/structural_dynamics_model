% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Indigenous Return Reading of Jewish Self-Determination
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the indigenous-return reading of the
 *   jewish_self_determination kernel: the claim that Jewish people hold
 *   unbroken indigenous connection to the land, such that Zionism constitutes
 *   decolonization rather than colonization. The reading treats indigeneity
 *   as a binary historical fact (an implied mountain — ancient origin is not
 *   a matter of degree) but its practical deployment functions as a rope-type
 *   coordination device: it coordinates Jewish national identity and
 *   international legitimacy claims around a single origin narrative while
 *   requiring active argumentative and institutional defense against the
 *   competing settler-colonial characterization, and it does so by
 *   subordinating a rival indigeneity claim. The contested classification is
 *   the point: accepted at face value the framework would carry near-zero
 *   extraction (indigenous status is asserted as binary and self-evident);
 *   once the classification itself is contested by rival historical and legal
 *   claims, defending it requires sustained institutional, legal, and
 *   rhetorical effort, and it produces a real cost borne by those whose
 *   competing indigeneity claim is thereby subordinated. This story authors
 *   ONLY the indigenous-return reading; the settler_colonial_reading,
 *   liberal_nationalist_reading, religious_covenant_reading, and
 *   diasporist_reading are separate constraints linked via
 *   network.affects_constraints, each with its own epsilon and stakeholder
 *   structure per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - jewish_claimants_to_ancestral_land: primary beneficiary of the indigeneity classification — grounds sovereignty claim in ancient continuous connection
 *   - israeli_state_institutions: agenda-setter deploying the reading in diplomatic, legal, and educational contexts
 *   - palestinian_indigeneity_claimants: bear the cost of having a competing indigeneity claim subordinated or reframed as later arrival
 *   - international_legal_and_historical_scholars: analytical observers assessing fit between ancient-origin-plus-diaspora-return and established indigenous-rights legal categories
 *   - settler_colonial_reading_advocates: excluded rival framework builders whose account this reading is explicitly constructed to rebut
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.68).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.55).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Indigenous Return Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political philosophy / nationalism studies / postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'f93f8a07-3799-4670-bb9e-7546882da5f0').
narrative_ontology:cs_kernel_codification('f93f8a07-3799-4670-bb9e-7546882da5f0', distributed).
narrative_ontology:cs_authority_grounding('f93f8a07-3799-4670-bb9e-7546882da5f0', distributed).
narrative_ontology:cs_reading_relation('f93f8a07-3799-4670-bb9e-7546882da5f0', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('f93f8a07-3799-4670-bb9e-7546882da5f0', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('f93f8a07-3799-4670-bb9e-7546882da5f0', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('f93f8a07-3799-4670-bb9e-7546882da5f0', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('f93f8a07-3799-4670-bb9e-7546882da5f0', foundational, ancient_continuous_connection_confers_indigenous_status).
narrative_ontology:cs_axiom_status(ancient_continuous_connection_confers_indigenous_status, holdable).
narrative_ontology:cs_axiom_grounding('f93f8a07-3799-4670-bb9e-7546882da5f0', ancient_continuous_connection_confers_indigenous_status, empirically_contingent).
narrative_ontology:cs_axiom('f93f8a07-3799-4670-bb9e-7546882da5f0', foundational, indigenous_status_recategorizes_return_as_decolonization_not_conquest).
narrative_ontology:cs_axiom_status(indigenous_status_recategorizes_return_as_decolonization_not_conquest, holdable).
narrative_ontology:cs_axiom_grounding('f93f8a07-3799-4670-bb9e-7546882da5f0', indigenous_status_recategorizes_return_as_decolonization_not_conquest, conventional).
narrative_ontology:cs_reference_frame('f93f8a07-3799-4670-bb9e-7546882da5f0', ancient_jewish_sovereignty_in_the_land).
narrative_ontology:cs_drift_state('f93f8a07-3799-4670-bb9e-7546882da5f0', post_1967_and_settlement_expansion_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f93f8a07-3799-4670-bb9e-7546882da5f0', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, zionism_as_decolonization_thesis).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_indigenous_connection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that continuous religious, linguistic, archaeological, and genealogical ties to the land since antiquity establish indigenous status. This reading grounds their claim to return and sovereignty in indigeneity rather than in international-law refugee mechanisms or colonial-era mandates, and is invoked to answer accusations of settler-colonialism directly on indigeneity's own terms.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, civilizational, constrained, national).

% Deploy the indigenous-return framing in diplomatic, legal, educational, and public-relations contexts to characterize state formation and continued settlement expansion as restoration rather than conquest. Curricula, foreign-ministry messaging, and legal briefs in international fora draw on this reading to resist the settler-colonial characterization and its associated remedies (return, restitution, decolonization frameworks).
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, beneficiary).

% Assert continuous, uninterrupted physical presence and cultivation of the land for many centuries and object to being recast as later arrivals, as a subordinate co-indigenous population, or as a population whose displacement claims are structurally weakened once the other party is classified as the primary indigenous returnee. The reading directly reduces the political and legal weight given to their own dispossession narrative and land claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants, payer,
    powerless, generational, trapped, national).

% Some draw identity and political solidarity from the indigenous-return framing without living under its consequences; others (aligned with diasporist readings) are sidelined when this reading becomes the dominant public account of Jewish self-determination, since it forecloses a diaspora-centered account of Jewish flourishing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, excluded).

% Evaluate competing indigeneity claims using archaeology, demography, comparative settler-colonial studies, and international law frameworks (e.g. UN indigenous rights instruments). Many note that 'indigenous' as a legal-political category was developed for contexts of recent (post-1500) colonization by identifiably foreign powers and does not map cleanly onto claims of ancient origin combined with millennia of intervening diasporic and local demographic change on both sides.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_legal_and_historical_scholars, observer,
    analytical, civilizational, analytical, global).

% Hold a directly competing account (Zionism as European settler-colonial project) and are excluded from co-authoring the indigenous-return narrative's institutional deployment; their framework is the one this reading is explicitly built to rebut and displace in international discourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, settler_colonial_reading_advocates, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, historically-grounded account of Jewish peoplehood and territorial claim that unifies religious, ethnic, and national dimensions of Jewish identity around a single origin narrative, and offers a ready rebuttal to the settler-colonial characterization in legal, diplomatic, and educational contexts.
% TRANSFER_FUNCTION: Moves legitimacy and rhetorical/legal standing from Palestinian dispossession claims toward Jewish sovereignty claims by recategorizing the underlying historical relationship: what settler-colonial framing treats as conquest and displacement, this reading treats as indigenous restoration, shifting the burden of justification onto counter-claims of Palestinian primacy.
% ABSENT_VOICES: Palestinian historians and communities who hold their own indigeneity claim are structurally positioned as respondents to this reading rather than co-authors of the indigeneity question; settler-colonial-reading scholars are excluded from shaping how 'indigenous' is defined and applied here, despite building the competing framework this reading directly targets.
% DISAPPEARANCE_RATIONALE: Israeli state legitimacy would not evaporate without this reading (other readings — liberal nationalist, religious covenant — offer alternative grounds), but the specific rhetorical and legal defense against 'settler-colonial' characterization would lose one of its principal tools, and diplomatic messaging strategies built around indigeneity claims (e.g. before UN bodies) would need to be rebuilt on different grounds. Whether this constitutes 'world rearranges' or 'world unchanged' is itself contested between parties who see the reading as load-bearing and those who see it as one rhetorical option among several.
% FOUNDING_PROBLEM: The postcolonial-era delegitimization of Zionism as a European colonial project (accelerating from the 1960s-70s 'Zionism is racism' era through contemporary decolonization discourse) needed a rebuttal framework capable of contesting the colonizer/colonized binary on its own conceptual terrain rather than rejecting the terrain itself.
% FOUNDING_PROBLEM_CORROBORATION: Israeli government communications, hasbara organizations, and aligned academic centers attest the framing addresses an ongoing and intensifying delegitimization campaign. Independent corroboration from outside directly benefiting parties is thinner: comparative settler-colonial studies scholars (e.g. writing in journals of Native American and Indigenous studies) and international law scholars note the debate over whether ancient-origin-plus-diaspora-return fits established indigenous-rights legal categories remains genuinely unresolved in the scholarly literature, which is a weaker form of corroboration than an endorsement of the reading itself — it corroborates that the question is live, not that this reading resolves it correctly.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate-to-high (0.68 by 2024) not because ancient historical origin claims are themselves extractive, but because the CONTESTED CLASSIFICATION function — using indigeneity status to determine whose dispossession narrative carries legal and moral weight in an active territorial and political conflict — has real distributive stakes. Suppression (0.55) reflects the active argumentative, legal, and institutional labor required to sustain the classification against a well-organized rival framework (settler_colonial_reading) and against a rival indigeneity claim from Palestinian communities; a genuinely uncontested mountain-type historical fact would not require this level of active defense. Resistance is high (0.82) precisely because rival historical and legal scholarship contests the framework's application, not its underlying facts about ancient Jewish presence (which are largely uncontested) but its inference to exclusive or primary indigenous status justifying present-day political and territorial consequences. Accessibility collapse is authored low-moderate (0.35): alternative framings (liberal nationalist, religious covenant, diasporist, settler colonial) remain live and actively argued, meaning the indigenous-return reading has not collapsed the space of alternatives the way a genuine mountain would.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat this reading looks like recovered historical truth correcting a defamatory colonial mislabeling. From the payer seat (Palestinian indigeneity claimants) the identical classificatory apparatus looks like an appropriation of the indigeneity category itself, redeployed to delegitimize their own presence and dispossession claims. The engine computes these as structurally different experiences of the same authored data — the story does not adjudicate which reading is correct, only that the structural asymmetry (one party's claim is elevated, the other's is subordinated, through the same classificatory move) is real and authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants to ancestral land and Israeli state institutions sit near the beneficiary end: the reading directly supplies legitimacy and rhetorical defense they use. Palestinian indigeneity claimants sit near the target end: the same classificatory move that establishes Jewish indigenous status is structurally paired, in this reading's own logic, with subordinating or historicizing the competing Palestinian indigeneity claim — this is why they are named as victims rather than merely as an excluded party. Diaspora Jewish communities are split: some benefit from the identity coherence the reading offers, while diasporist-aligned diaspora communities are structurally sidelined when this reading dominates public discourse about what Jewish self-determination requires.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rebutting delegitimization-through-colonial-framing) remains live by the beneficiaries' own account, but corroboration from outside the benefiting parties is thin and specifically about the liveness of the underlying legal-categorical question, not an endorsement that this reading resolves it. This is the honest mismatch the R5 interview is designed to surface: a status of 'live' resting mostly on self-attestation from parties who structurally benefit from the reading's continued deployment, with independent scholarship corroborating only that the categorical question is unsettled — not that this particular resolution is correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_category_applicability,
    'Does the international legal/political category of ''indigenous peoples'' (developed primarily for post-1500 colonization by identifiably foreign, geographically distant colonial powers) meaningfully apply to a claim of ancient origin combined with roughly two millennia of diaspora and return, where the counter-party also has a plausible continuous-presence claim over much of the same period?',
    'Comparative analysis against how indigenous-rights frameworks (e.g. ILO Convention 169, UNDRIP) have been applied or refused in other cases of ancient-origin-plus-return claims; scholarly consensus-building in comparative settler-colonial and indigenous-studies literature; historical demographic and archaeological research bearing on continuity of presence for both populations.',
    'If the category does not fit, the entire indigenous-return reading collapses as a category error regardless of the truth of the underlying historical facts about ancient Jewish presence, and the constraint would need to be re-grounded (e.g. in liberal_nationalist_reading or religious_covenant_reading terms) rather than in indigeneity terms specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigeneity_category_applicability, conceptual, 'Whether the indigenous-peoples legal-political category coherently applies to this historical situation at all.').

omega_variable(
    binary_vs_contested_classification,
    'Is indigenous status here a binary historical fact (as this reading''s own logic requires — it is either true or false that Jewish people are indigenous) or is it an inherently contested political classification with no fact-of-the-matter resolution?',
    'Track whether historiographical consensus converges over time (suggesting a binary fact awaiting discovery) or remains persistently split along political-identity lines regardless of new evidence (suggesting the classification is irreducibly political/interpretive).',
    'If genuinely binary and resolvable, epsilon should be low (per the expected structural delta) once resolved — the mountain reading would be vindicated. If irreducibly contested, epsilon stays high indefinitely because the classification will always require active political and legal defense — the rope/tangled-rope reading is the stable long-run structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_vs_contested_classification, conceptual, 'Whether indigenous status is a discoverable fact or a persistently contested political classification.').

omega_variable(
    co_indigeneity_possibility,
    'Can both Jewish and Palestinian populations hold valid, non-hierarchical indigenous claims simultaneously (co-indigeneity), or does the framework require ranking one claim as primary and the other as subordinate or non-indigenous?',
    'Examine comparative cases of overlapping indigenous claims elsewhere (e.g. multiple indigenous nations with historical claims to overlapping territory) to see whether legal and political frameworks have successfully implemented non-hierarchical co-indigeneity, and whether either side''s advocates would accept such a framework here.',
    'If co-indigeneity without hierarchy is achievable, this reading''s victim declaration (palestinian_indigeneity_claimants bearing subordination cost) could be resolved without abandoning the Jewish indigeneity claim, substantially lowering epsilon. If the framework is inherently zero-sum as currently deployed, the extraction is structural to the reading as actually used, not incidental to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_indigeneity_possibility, empirical, 'Whether the indigeneity framework can be structured as non-exclusive rather than as a ranking mechanism.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does this reading diverge from settler_colonial_reading — is it a disagreement over historical facts (continuity of presence, nature of early Zionist settlement), over which legal category applies (indigenous vs. colonial-settler), or over the normative weight assigned to ancient origin versus recent continuous habitation?',
    'Structural decomposition of the two readings'' factual claims versus their categorical/normative claims, to identify whether the readings could converge on facts while remaining opposed on categorization and normative weighting.',
    'If the disagreement is primarily categorical/normative rather than factual, this clarifies that the two readings are not actually contesting the same evidentiary record and cannot be resolved by historical research alone — routing further inquiry toward political philosophy rather than historiography.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the precise structural point of disagreement between the indigenous_return_reading and settler_colonial_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(jewi_tr_t1988, jewish_self_determination__indigenous_return_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__indigenous_return_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(jewi_tr_t2012, jewish_self_determination__indigenous_return_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(jewi_be_t1988, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1988, 0.5).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(jewi_be_t2012, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1967, 0.38).
narrative_ontology:measurement(jewi_su_t1988, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1988, 0.45).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(jewi_su_t2012, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the jewish_self_determination kernel, decomposed per the epsilon-invariance principle because the natural-language question 'is Zionism decolonization or colonization' conflates structurally distinct claims (indigeneity-grounded, nationalist-grounded, covenant-grounded, diaspora-pluralist, and settler-colonial-critical) each with different epsilon, different beneficiary/victim structure, and different type. This reading (indigenous_return_reading) and settler_colonial_reading are the most directly opposed pair — they contest the same territorial claim using structurally incompatible categorizations of the same historical relationship (restoration vs. dispossession) — while liberal_nationalist_reading and religious_covenant_reading offer alternative, less contested grounds for the same political conclusion, and diasporist_reading rejects the territorial-sovereignty framing shared by all the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
