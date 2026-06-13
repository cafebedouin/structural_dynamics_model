% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Substrate (Indigenous Sovereignty Reading)
 *   domain: legal/constitutional/indigenous
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the historical treaty
 *   substrate between Indigenous nations and settler states. Under this
 *   reading, treaties are binding international agreements between sovereign
 *   equals, requiring ongoing consent to territorial changes and resource
 *   extraction. Indigenous nations are beneficiaries—they enter the framework
 *   as holders of retained sovereignty with veto rights over major
 *   modifications. Settler states are constrained—they become subject to
 *   international law obligations. Resource-extraction industries pay—they
 *   lose frictionless access to treaty territories and must negotiate
 *   Indigenous consent. The claim/metric independence is deliberate: this
 *   reading is CLAIMED as tangled_rope (genuine coordination of coexistence +
 *   asymmetric extraction through institutional burden) while metrics show
 *   substantial suppression (0.71) and moderate theater (0.42). The
 *   suppression reflects settler-state institutional machinery (courts,
 *   legislatures, administrative agencies) that can constrain or perform the
 *   reading; the theater reflects the growing gap between declared consent
 *   processes and actual Indigenous involvement in resource decisions. The
 *   measurement series shows rising suppression from t0 to t37 as the reading
 *   gained institutional salience and settler states hardened enforcement
 *   machinery against Indigenous claims, then partial decline at t50 as some
 *   Indigenous legal victories opened institutional space. This is one member
 *   of a three-reading kernel family (extinguishment_reading,
 *   stewardship_reading); it is NOT the only defensible frame and must be
 *   authored as a structural choice, not a settled fact.
 *
 * KEY AGENTS:
 *   - indigenous_nations: holders of retained sovereignty under this reading; beneficiaries of ongoing-consent principle; identity-locked to territorial sovereignty claims.
 *   - settler_state_government: agenda-setter administering treaty interpretation; constrained by international law obligations; pays through loss of unilateral authority.
 *   - resource_extraction_industries: powerful actors facing heightened friction accessing treaty territories; must absorb consent-negotiation costs or relocate.
 *   - international_treaty_law_framework: vindicated proposition (not a collecting beneficiary); legitimate only if this reading's core premise holds.
 *   - lower_court_judges: moderate-power payers; must adopt interpretive premises that constrain ruling discretion.
 *   - settler_state_legislatures: excluded; their preferred reading (unilateral authority) is incompatible with this frame.
 *   - colonial_historical_establishment: excluded; the interpretive apparatus that treated treaties as completed transactions is structurally absent from this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.58).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Substrate (Indigenous Sovereignty Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/constitutional/indigenous").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '9435640b-40a3-4b1c-b44e-0827bd76854c').
narrative_ontology:cs_kernel_codification('9435640b-40a3-4b1c-b44e-0827bd76854c', fixed_text).
narrative_ontology:cs_authority_grounding('9435640b-40a3-4b1c-b44e-0827bd76854c', lineage).
narrative_ontology:cs_interpretation_layer_present('9435640b-40a3-4b1c-b44e-0827bd76854c').
narrative_ontology:cs_reading_relation('9435640b-40a3-4b1c-b44e-0827bd76854c', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('9435640b-40a3-4b1c-b44e-0827bd76854c', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('9435640b-40a3-4b1c-b44e-0827bd76854c', foundational, indigenous_nations_retain_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_retain_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9435640b-40a3-4b1c-b44e-0827bd76854c', indigenous_nations_retain_sovereignty, deontological).
narrative_ontology:cs_axiom('9435640b-40a3-4b1c-b44e-0827bd76854c', foundational, treaty_modification_requires_mutual_consent).
narrative_ontology:cs_axiom_status(treaty_modification_requires_mutual_consent, holdable).
narrative_ontology:cs_axiom_grounding('9435640b-40a3-4b1c-b44e-0827bd76854c', treaty_modification_requires_mutual_consent, conventional).
narrative_ontology:cs_axiom('9435640b-40a3-4b1c-b44e-0827bd76854c', secondary, vienna_convention_good_faith_performance).
narrative_ontology:cs_axiom_status(vienna_convention_good_faith_performance, holdable).
narrative_ontology:cs_axiom_grounding('9435640b-40a3-4b1c-b44e-0827bd76854c', vienna_convention_good_faith_performance, conventional).
narrative_ontology:cs_reference_frame('9435640b-40a3-4b1c-b44e-0827bd76854c', indigenous_sovereignty_retained_post_signature).
narrative_ontology:cs_drift_state('9435640b-40a3-4b1c-b44e-0827bd76854c', contemporary_post_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9435640b-40a3-4b1c-b44e-0827bd76854c', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_legal_community).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_resource_extraction_interests).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, non_indigenous_resource_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, lower_court_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treated under this reading as co-equal sovereigns retaining territorial jurisdiction and requiring ongoing consent to resource extraction and territorial changes. They benefit from treaty recognition as international agreements (not unilateral grants) and from the principle that modifications require their consent. They pay through the constraint's capacity to channel disputes through litigation and the burden of defending sovereignty claims in institutional forums. Their territorial identity is fused with sovereignty claims—exit means abandoning the foundation of nation-hood.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer).

% Administers treaty interpretation through domestic courts and executive branches. Under this reading, constrained by international law principles requiring good-faith performance of treaty obligations and ongoing consent of Indigenous treaty parties. Sets enforcement machinery (court process, regulatory frameworks) but cannot unilaterally rewrite the substrate without violation. Ability to extract resources from treaty territories becomes contingent on demonstrated Indigenous consent.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% Operating under this reading, face heightened friction accessing treaty territories because resource projects require Indigenous nation consent, not merely settler-state licensing. Extraction that proceeded under prior readings (unilateral state grant, exhausted extinguishment) becomes legally contestable. Exit involves relocating operations or absorbing consent-negotiation costs. They bear the cost of the constraint's enforcement—blocking or delaying projects that lack Indigenous agreement.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries, payer,
    powerful, biographical, mobile, global).

% This reading vindicates the principle that treaties are binding international agreements requiring performance in good faith (Vienna Convention on the Law of Treaties), that sovereign parties retain consent rights, and that unilateral modification violates international law. No actor collects from this doctrine, but its legitimacy—its status as the operative frame—is what the constraint enforces.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_law_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, international_treaty_law_framework).

% Enforcing this reading in domestic litigation requires adopting interpretive premises (Indigenous nations retain sovereignty, treaties are ongoing contracts requiring consent) that constrain how they rule on resource and land disputes. The interpretation layer—the work of reasoning through treaty text, Indigenous intent, and international law principles—becomes mandatory rather than optional. They bear the cost of institutional constraint.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, lower_court_judges, payer,
    moderate, biographical, constrained, national).

% Would ordinarily have authority to legislate territorial and resource policy unilaterally; this reading excludes them from rewriting treaties without Indigenous consent. Legislating around or beneath treaty obligations becomes contestable. They are absent from the initial framing but would object strongly to the constraint's implications—that their legislative supremacy is bounded by binding international agreements where they are not sole parties.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_legislatures, excluded,
    institutional, generational, constrained, national).

% The institutional and narrative structure that treated treaties as unilateral grants or property transactions—the interpretation that legitimated 150+ years of settler resource extraction—is structurally excluded from this reading's framing. It remains operative in parallel institutional contexts but cannot claim equal validity under modern treaty law.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, colonial_historical_establishment, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, colonial_historical_establishment).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_government).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for resolving territorial disputes between two unequal parties (Indigenous nations historically without military parity, settler states with institutional machinery) by anchoring both to a written agreement whose terms are binding and whose modification requires mutual consent. Solves the coordination problem of coexistence without mutual annihilation or absolute dominance by creating a shared legal reference.
% TRANSFER_FUNCTION: Transfers interpretive authority from settler-state unilateral decision-making to a bilateral framework requiring Indigenous consent for significant resource extraction and territorial changes. Moves the burden of proof from Indigenous nations (who must mount legal challenges to contested extraction) to settler states (who must demonstrate consent for disputed projects). Transfers institutional leverage from legislatures to courts enforcing treaty obligations.
% ABSENT_VOICES: Settler-state legislatures and resource-extraction industries (excluded because their preferences—unilateral authority over territory and resources—are structurally incompatible with this reading). Colonial historical actors and institutions whose legitimacy rested on treating treaties as completed transactions are absent from the framing. Non-Indigenous settlers whose property or economic interests depend on territories claimed under prior treaty readings are not seats in the initial negotiation.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement mechanisms vanished, settler states would revert to unilateral resource extraction and territorial policy on treaty lands; Indigenous nations would lose the legal framework anchoring their sovereignty claims; resource industries would operate without consent-negotiation friction. The settlement patterns, institutional arrangements, and resource allocation across dozens of territories would reorganize around the previous reading (extinguishment or stewardship frames). Territories currently under dispute would shift from plural-sovereignty conflict to settler-state authority.
% FOUNDING_PROBLEM: Historical treaties between Indigenous nations and settler states were interpreted by settler courts as completed transactions, extinguishing Indigenous sovereignty and leaving only reserve trust responsibilities—a one-time reading that ignored the ongoing consent principles embedded in the actual treaty texts and in international law. Indigenous nations were systematically denied standing to interpret their own treaty obligations, and unilateral settler extraction proceeded without Indigenous consent.
% FOUNDING_PROBLEM_CORROBORATION: International treaty law scholars and human rights bodies (external to both Indigenous nations and settler governments) document systematic misinterpretation of treaty language and violation of Vienna Convention principles. Indigenous treaty archivists and legal scholars (internal but subordinated in prior readings) have long documented that treaty texts themselves embed ongoing-consent language. Settler-state supreme courts (e.g., Canada's Delgamuukw decision, Australia's Mabo decision) have begun acknowledging the problem; independent historical analysis shows the extinguishment reading was imposed by administrative convenience, not treaty text.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the constraint extracts from resource-extraction interests but genuinely coordinates coexistence for both parties—neither can unilaterally impose terms without violation. Suppression is elevated (0.71) because settler-state machinery—court systems, administrative review, regulatory capture—consistently narrows the scope of Indigenous consent rights in practice, even when the principle is formally acknowledged. The measurement series show rising suppression from t0 to t37, reflecting the period (roughly 1990–2015) when Indigenous legal victories opened consent-requirement discourse but settler-state institutional responses hardened. The peak at t37 reflects maximum friction—the principle is established enough that settler states must defend their choices through elaborate interpretive machinery, but enforcement is inconsistent. The slight decline at t50 reflects some Indigenous institutional capacity gains (legal precedents, international advocacy momentum), suggesting the suppression trajectory is not monotonic but oscillates with institutional pressure. Theater (0.42) is moderate: the constraint involves real litigation, genuine consent discussions (sometimes), and serious treaty-interpretation scholarship—but also performative consultation processes where Indigenous input is solicited but extraction proceeds regardless, and settler-state framing of consultation as stakeholder management rather than sovereign negotiation. The accessibility_collapse (0.68) reflects that alternatives to the nation-to-nation frame exist (stewardship reading, extinguishment reading) but are increasingly costly to maintain as international law and Indigenous advocacy networks make this reading salient. Resistance is high (0.73) because resource industries mount vigorous legal challenges and settler-state legislatures attempt jurisdictional carve-outs.
 *
 * PERSPECTIVAL GAP:
 *   The settler_state_government and lower_court_judges seats should compute as constrained or moderately extractive—they see the reading as limiting their authority and carrying institutional costs. The indigenous_nations seat should compute as beneficiary or near-symmetric—they see genuine gains in sovereignty recognition and procedural leverage, offset by the institutional suppression that narrows the principle in practice. The resource_extraction_industries seat should compute as targeted—they experience unambiguous extraction in the form of consent-negotiation friction and delayed or blocked projects. The international_treaty_law_framework (non-agent) has no directionality—it is a vindicated proposition that the constraint's operation legitimates. This reading-specific divergence (same constraint, different perceived types across seats) is exactly what the engine computes; the authored metrics do not adjudicate which seat's experience is 'true.'
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations enter as primary beneficiaries (d near 0.0)—the reading affirms their sovereignty and requires settler-state consent-seeking. They also pay a secondary cost: institutional suppression and the burden of litigating against well-resourced settler states. The identity_locked exit option amplifies their directionality—they cannot exit the sovereignty claim framework without abandoning nation-hood itself, so even high institutional friction does not enable escape. Settler state government is the agenda-setter (high power, institutional scope) but is constrained by the reading—it cannot unilaterally rewrite treaty terms. Its directionality is moderate (d ~0.45), pulled toward target by loss of unilateral authority, pulled toward beneficiary by retained governance capacity. Resource-extraction industries are clear targets (d near 1.0)—they have mobile exit options and face direct friction from consent requirements. Lower-court judges are moderate-power payers (d ~0.55)—they bear interpretive constraint burden. The settler-state legislatures and colonial-historical establishment are excluded (not modeled in directionality) because they are structurally incompatible with this reading's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status = live, disappearance_verdict = world_rearranges mismatch is the key mandatrophy signal. This reading asserts that the founding problem (misinterpretation of treaties as one-time extinguishment) is still operative, and that if the reading vanished, territorial arrangements would reorganize back toward settler-state unilateral authority. The mismatch analysis: if status were dead (problem solved, proper interpretation established), world_rearranges would indicate the reading itself has become unnecessary—a sunset candidate. Because status is live, world_rearranges indicates the constraint is defending against ongoing settler-state drift toward the old reading. The theater measurement (moderate at 0.42, rising to 0.44 by t37) confirms this: the constraint is real (not pure performance) but carries performative elements (consultation machinery that doesn't structurally enforce). The theater rise mirrors suppression rise, consistent with the interpretation that settler states respond to Indigenous legal wins by adding procedural machinery (more consultation) while hardening substantive denial (more skilled rejection of consent claims). No mandatrophy resolution yet—the reading is still defending an interpretation against institutional pressure, not maintaining a solved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hist_tr_t0, observed).
narrative_ontology:measurement(hist_tr_t8, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement_basis(hist_tr_t8, observed).
narrative_ontology:measurement(hist_tr_t16, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(hist_tr_t16, observed).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(hist_tr_t25, observed).
narrative_ontology:measurement(hist_tr_t37, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 37, 0.44).
narrative_ontology:measurement_basis(hist_tr_t37, observed).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(hist_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hist_be_t0, observed).
narrative_ontology:measurement(hist_be_t8, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(hist_be_t8, observed).
narrative_ontology:measurement(hist_be_t16, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(hist_be_t16, observed).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(hist_be_t25, observed).
narrative_ontology:measurement(hist_be_t37, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 37, 0.63).
narrative_ontology:measurement_basis(hist_be_t37, observed).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(hist_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hist_su_t0, observed).
narrative_ontology:measurement(hist_su_t8, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(hist_su_t8, observed).
narrative_ontology:measurement(hist_su_t16, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(hist_su_t16, observed).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(hist_su_t25, observed).
narrative_ontology:measurement(hist_su_t37, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 37, 0.79).
narrative_ontology:measurement_basis(hist_su_t37, observed).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(hist_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.14).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, settler_state_resource_extraction_authority).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, indigenous_territorial_jurisdiction).

% DUAL FORMULATION NOTE:
% Part of the historical_treaty_substrate kernel family. This reading (nation-to-nation) structures Indigenous sovereignty and settler-state constraint. The sibling extinguishment_reading structures one-time cession and settler-state establishment; the stewardship_reading structures relational coexistence. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and type. They are linked through network.affects_constraints because institutional adoption of one reading shifts the legitimacy conditions for the others—a settler-state court ruling favoring nation-to-nation narrows the scope of extinguishment claims, and vice versa. Do not attempt to merge the readings into one constraint with observable-dependent ε; that would violate ε-invariance. Each reading is an independent constraint from a distinct structural framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
