% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Enforcement as Restriction on Fundamental Freedom of Movement
 *   domain: political/international law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the FREEDOM-OF-MOVEMENT READING of the
 *   border-legitimacy kernel. It asserts that freedom of movement is a
 *   fundamental human right and border enforcement is a presumptively
 *   illegitimate restriction on that right. Under this reading, borders
 *   extract economic surplus from excluded populations by restricting labor
 *   supply and opportunity, and extract power/legitimacy from states that
 *   deploy borders as primary governance infrastructure. The reading accepts
 *   that rival readings (sovereignty doctrine, humanitarian carve-outs) exist
 *   and are held by other institutional actors, but characterizes those
 *   readings as rationalizations for an inherently extractive constraint. The
 *   claim/metric gap is intentional: border enforcement is CLAIMED (by
 *   states, incumbent workers, security apparatus) as legitimate
 *   security/welfare protection; the freedom-of-movement reading MEASURES it
 *   as substantially extractive (ε=0.78), actively enforced
 *   (suppression=0.81), with rising theatrical justification (theater rising
 *   from 0.32 to 0.44 over the interval as climate/conflict migration
 *   increases pressure). The engine computes the divergence per seat—how the
 *   security apparatus perceives this as coordination while excluded
 *   populations perceive it as snare.
 *
 * KEY AGENTS:
 *   - economic_migrants (powerless/trapped, victims of restriction): lack arbitrage-grade exit; return to origin is blocked by economic/climate conditions
 *   - incumbent_citizens_wage_earners (organized/constrained, beneficiaries via protection): benefit from labor-supply restriction; constrained exit because relocation abroad costs social capital
 *   - displaced_workers_climate_conflict (powerless/identity_locked, core victims): displacement is irreversible; identity as 'displaced' fuses with their situation
 *   - state_security_apparatus (institutional/analytical, agenda-setter and beneficiary): administers enforcement, collects power/budget from borders
 *   - nationalist_political_movements (organized/mobile, beneficiaries): gain mobilization energy and narrative frame from border legitimacy crisis
 *   - supranational_human_rights_bodies (institutional/analytical, observer): codify the reading, create legal contradiction within state systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.81).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement as Restriction on Fundamental Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political/international law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '6e3a7749-5f72-4a81-9009-7aa8c70a145e').
narrative_ontology:cs_kernel_codification('6e3a7749-5f72-4a81-9009-7aa8c70a145e', fixed_text).
narrative_ontology:cs_authority_grounding('6e3a7749-5f72-4a81-9009-7aa8c70a145e', lineage).
narrative_ontology:cs_interpretation_layer_present('6e3a7749-5f72-4a81-9009-7aa8c70a145e').
narrative_ontology:cs_reading_relation('6e3a7749-5f72-4a81-9009-7aa8c70a145e', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e3a7749-5f72-4a81-9009-7aa8c70a145e', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('6e3a7749-5f72-4a81-9009-7aa8c70a145e', foundational, freedom_of_movement_is_presupposed_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_presupposed_right, holdable).
narrative_ontology:cs_axiom_grounding('6e3a7749-5f72-4a81-9009-7aa8c70a145e', freedom_of_movement_is_presupposed_right, deontological).
narrative_ontology:cs_axiom('6e3a7749-5f72-4a81-9009-7aa8c70a145e', foundational, state_territorial_authority_cannot_override_presupposed_freedoms).
narrative_ontology:cs_axiom_status(state_territorial_authority_cannot_override_presupposed_freedoms, holdable).
narrative_ontology:cs_axiom_grounding('6e3a7749-5f72-4a81-9009-7aa8c70a145e', state_territorial_authority_cannot_override_presupposed_freedoms, deontological).
narrative_ontology:cs_reference_frame('6e3a7749-5f72-4a81-9009-7aa8c70a145e', universal_human_freedom_presumption).
narrative_ontology:cs_drift_state('6e3a7749-5f72-4a81-9009-7aa8c70a145e', contemporary_climate_migration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e3a7749-5f72-4a81-9009-7aa8c70a145e', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, incumbent_citizens_via_wage_protection).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, nationalist_political_movements).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients_in_open_movement_scenario).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, refugees_outside_humanitarian_carve_outs).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, stateless_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, incumbent_citizens_wage_earners).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_climate_conflict).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, origin_region_governments).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, national_security_framework).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, welfare_state_closure_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek entry to higher-wage labor markets and are systematically excluded by border enforcement. Their economic situation (low-wage origin region) makes exit from that region the primary rational choice, but borders block that exit. They cannot negotiate exit terms, have no political voice in border policy, and bear severe costs (exploitative smuggling, deportation, family separation) when attempting unauthorized crossing.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Persons displaced by climate change, resource collapse, armed conflict, or state failure. Their displacement is often irreversible (return is impossible or unsafe). Under the freedom-of-movement reading, border closure against them is extractive—they are expelled from potential safety by borders claimed as legitimate. Their identity as 'displaced' is inseparable from their current situation, making psychological/institutional exit from the displaced category difficult even when geographic exit is theoretically possible.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_climate_conflict, payer,
    powerless, biographical, identity_locked, global).

% Benefit from border enforcement that restricts labor supply in their home labor markets, suppressing wage competition from lower-wage workers. They frame the benefit as cultural/security protection but materially benefit from reduced competition. Exit is available (emigration) but costly in social capital and family ties, making it constrained rather than mobile. Under the freedom-of-movement reading, they are incidental beneficiaries of an illegitimate restriction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, incumbent_citizens_wage_earners, beneficiary,
    organized, biographical, constrained, national).

% Administers border enforcement infrastructure and derives budget, authority, and institutional relevance from border operations. Controls enforcement intensity, technological deployment, and regulatory interpretation. Under the freedom-of-movement reading, the apparatus is a primary beneficiary whose interest in border persistence drives continued enforcement even as the founding security justification decays.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_security_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, state_security_apparatus, beneficiary).

% Gain political mobilization energy and narrative framing from border legitimacy discourse. They advocate for border enforcement on cultural and security grounds, which serves as their primary political platform against cosmopolitan/internationalist opponents. They are beneficiaries of the constraint's persistence because the constraint's contested legitimacy sustains their political relevance and electoral appeal.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, nationalist_political_movements, beneficiary,
    organized, biographical, mobile, national).

% Document freedom-of-movement violations and issue rulings asserting the right to freedom of movement. They cannot directly override state enforcement but they codify the freedom-of-movement reading and create internal contradictions within state legal structures by positioning borders as violations of international law. They serve as the primary institutional voice for this reading.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, supranational_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Operate welfare systems designed as citizen-exclusive and would face difficult choices if borders opened and welfare access became contested. They are excluded from border policy conversations even though border policy directly affects their resource constraints. They contend (falsely, under the freedom-of-movement reading) that open borders forces a choice between universal inclusion and welfare collapse—a claim the reading challenges as a manufactured constraint serving political convenience.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_welfare_administrators, excluded,
    organized, biographical, constrained, national).

% Lose productive population and human capital to emigration but are systematically blamed for 'causing' migration by border-enforcing states, shifting responsibility away from border regimes. They have political voice but limited enforcement capacity and cannot directly affect receiving-state border policy. They are partially victimized by the asymmetry: their populations are excluded while they are blamed for exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, origin_region_governments, payer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The freedom-of-movement reading asserts that border enforcement is pure restriction with no coordination benefit. It divides a single global labor market into protected and unprotected segments and prevents voluntary reallocation of human capital to uses of higher value. No collective-action problem is solved; instead, an artificial collective-action problem is created (how to enforce restriction against those seeking to migrate).
% TRANSFER_FUNCTION: Transfers opportunity value and economic surplus from excluded populations (primarily in low-wage origin regions) to incumbent workers in high-wage receiving regions via labor-supply restriction. Transfers power, authority, and budget to the state security apparatus via border administration. Transfers political capital to nationalist movements via legitimacy discourse around borders.
% ABSENT_VOICES: Persons in origin regions without sufficient political power to voice objections to receiving-state border policy (their home governments have voice but limited enforcement capacity); future generations whose climate displacement will challenge the legitimacy of border closure; stateless persons whose voices are excluded a priori by state closure; persons not yet born in high-wage regions who will experience border restrictions if they are born elsewhere.
% DISAPPEARANCE_RATIONALE: If border enforcement vanished overnight, global labor reallocation would accelerate rapidly, wage convergence across regions would begin within years, incumbent-worker wage premiums would compress, the state security apparatus would lose its primary jurisdictional justification and face institutional collapse or radical transformation, and nationalist political movements would lose their primary mobilization frame and electoral appeal. The current global distribution of wealth, opportunity, and power depends structurally on borders; borders are not a side effect or auxiliary mechanism but a primary extraction and power-concentration system.
% FOUNDING_PROBLEM: Border enforcement was originally justified by state security concerns (military threat, espionage, contagion). Over time, justifications expanded to include labor-market protection (wage defense for incumbent workers) and welfare-state closure (citizen-exclusive benefits). The security framing persists as the primary legitimating narrative.
% FOUNDING_PROBLEM_CORROBORATION: The state security apparatus and nationalist movements attest that the founding security problem remains live and justifies borders. However, international human rights bodies, economic historians, migration scholars, and epidemiologists attest that modern intelligence systems, diplomacy, and public-health infrastructure address the original security concerns far more effectively than borders do; borders persist primarily as economic and political instruments rather than security responses. The freedom-of-movement reading's corroboration comes from outside the beneficiary seats: from excluded populations documenting the arbitrary and extractive nature of border closure; from economists documenting the massive welfare losses from labor-market segmentation; from legal scholars documenting the fundamental contradiction between universal human rights norms and territorial exclusion; and from international bodies formalizing the right to freedom of movement in universal law.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint transfers opportunity value from excluded populations to incumbent workers and the state apparatus with no coordination benefit—the constraint does not solve a collective-action problem but rather sustains an artificial scarcity. The measurement series traces extraction ACCUMULATION over the interval: it rises from 0.62 to 0.78, driven by intensifying climate/conflict displacement pressure that borders block without legitimate security justification. Suppression is high (0.81) because enforcement depends on active police/military/surveillance apparatus; excluded populations have no voice in border policy and face lethal consequences for breach. Theater rises from 0.32 to 0.44 because the narrative justification for borders (terrorism prevention, security) decouples further from operational reality (most border enforcement targets economic migrants, not security threats); the more obviously borders function as economic restriction, the more theatrical the security narrative becomes. Accessibility collapse is high (0.68) because once a person is born in a low-wage region or becomes displaced, their effective exit from that condition is blocked by borders—alternatives (remote work, internal migration) persist but are inadequate. Resistance is high (0.72) because excluded populations mount continuous resistance (migration attempts, smuggling networks, political pressure for asylum expansion) despite enormous coercive force.
 *
 * PERSPECTIVAL GAP:
 *   The state_security_apparatus and incumbent_citizens seats should compute coordination (legitimate security/labor protection) while economic_migrants and displaced_workers seats compute snare (illegitimate restriction). The engine derives this from structural data: the apparatus controls enforcement and faces no exit (analytical power), so its directionality is near beneficiary; economic migrants are trapped/powerless, so their directionality is near full target. The divergence is the measurement the corpus takes—a constraint that looks like coordination from one seat and extraction from another is exactly how institutional power asymmetry manifests.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are incumbent citizens (protected labor supply, suppressed wage competition) and the state apparatus (budget, authority, jobs from border administration). Victims are economic migrants (excluded from higher-wage markets), displaced workers (excluded from safety), and formally, welfare recipients in any open-movement scenario (if borders opened, welfare could be universal, shifting costs). The security apparatus is the agenda-setter: it decides enforcement intensity, technological investment, and operational rules. Rival payment networks equivalent to 'rival state systems' are excluded—they cannot compete for jurisdiction over population movement; border enforcement prevents that exit. Directionality for incumbent citizens is moderate (they benefit but also bear costs of border violence spillover, labor-market disruption from failed states); for economic migrants it is near 1.0 (full target—extraction with no offsetting benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling border enforcement as 'natural' (mountain) or 'coordinated' (rope). The constraint IS actively enforced (militarized borders, asylum denial, deportation apparatus). The constraint DOES extract from identifiable victims (excluded populations). The constraint's claimed founding problem (state security) is increasingly disconnected from its actual operation (labor-market protection, demographic control). Mandatrophy is LIVE: the constraint persists despite its founding security justification being substantially addressed by modern intelligence and diplomacy. The theatrical elaboration of border legitimacy (national identity, cultural protection, security theater) rises as the economic restriction function becomes more obvious. This is exactly the mandatrophy signal—the constraint persists by theater when its original function atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_border,
    'Is a border a naturally-emerged boundary (geographic, ethnic, cultural differentiation that precedes and justifies political borders) or a constructed partition imposed for state extraction?',
    'Genealogical analysis of border formation: historical records showing deliberate state imposition vs. organic boundary-setting by communities. Geographic analysis of whether natural barriers predict state borders or vice versa.',
    'If borders are constructed for extraction, the freedom-of-movement reading holds and ε is inherently high. If borders emerge naturally from community boundaries, the sovereignty reading has more force and extraction is justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_border, empirical, 'Whether borders are natural features or constructed for institutional benefit.').

omega_variable(
    welfare_closure_necessity,
    'Is welfare-state closure (citizens-only welfare) structurally necessary for the viability of redistribution, or is it a chosen limitation that could be overcome with universal systems?',
    'Comparative analysis of jurisdictions with universal welfare systems and open migration (Nordic countries pre-2015, some EU zones); natural experiments from welfare expansion and contraction during migration surges.',
    'If closure is unnecessary, welfare recipients are correctly identified as victims (they incur cost from potential universal inclusion that borders prevent). If closure is necessary, welfare recipients become incidental beneficiaries and the victim set shrinks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_closure_necessity, conceptual, 'Whether welfare and border closure are structurally coupled or contingently linked.').

omega_variable(
    security_vs_economic_restriction,
    'What proportion of border enforcement is devoted to genuine security (terrorism, weapons, contagion) vs. economic restriction (wage protection, labor-supply control)?',
    'Empirical audit of border agency resource allocation, personnel deployment, enforcement action outcomes, and conviction statistics. Cross-national comparative data on what borders actually apprehend.',
    'If security is <10% of enforcement, borders are pure economic extraction and the freedom-of-movement reading is vindicated. If security is substantial, borders perform a genuine coordination function and theater_ratio is lower than authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_vs_economic_restriction, empirical, 'The functional decomposition of border enforcement between security and economic restriction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression (0.81) of excluded populations structurally enforced (legal barriers, military/police, lack of pathways) or partly internalized (excluded populations internalize narratives of illegitimacy, construct self-exclusion through shame/identity)?',
    'Post-border-opening natural experiment: if suppression persists after legal barriers are removed (e.g., EU internal-freedom-of-movement cases), it is internalized. If suppression decays once legal barriers vanish, it was structural.',
    'If internalized, excluded populations carry the suppression with them even after exit becomes possible—the constraint''s grip is deeper. If structural, exit removes suppression immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether border suppression is structural or internalized in the target population.').

omega_variable(
    freedom_as_presupposition_vs_derived_claim,
    'Is freedom of movement a presupposed foundational right (prior to state authority, not something states can legitimately restrict) or a derived claim that requires justification and can be overridden by competing values?',
    'Jurisprudential review of international legal tradition (Universal Declaration, regional human rights instruments) and philosophical grounding. The framing cannot be empirically resolved—it is a foundational commitment.',
    'If freedom is presupposed, any border enforcement is prima facie illegitimate and the reading holds. If freedom is derived, borders may be justified if they serve compelling state purposes and this reading is one position in an ongoing contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_as_presupposition_vs_derived_claim, conceptual, 'Whether freedom of movement is foundational or derived.').

omega_variable(
    global_vs_local_extractiveness,
    'Is the extraction truly global (borders extract from people everywhere by restricting opportunity) or is the effect local (borders extract primarily from persons in low-wage regions)?',
    'Global income distribution analysis: if borders cause systematic global inequality (rich-region wage premiums sustained by border closure), extraction is global. If borders primarily affect already-poor regions (have little effect on high-wage region outcomes), extraction is regional.',
    'If global, the victim set is nearly universal and the extraction is massive. If regional, incumbent-worker protection in high-wage regions is a real benefit that partially offsets the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_vs_local_extractiveness, empirical, 'Whether border-driven extraction is distributed globally or concentrated regionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__freedom_of_movement_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__freedom_of_movement_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__freedom_of_movement_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__freedom_of_movement_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__freedom_of_movement_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__freedom_of_movement_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(bord_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(bord_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement_basis(bord_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.05).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, labor_market_segmentation).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, state_apparatus_authority_legitimacy).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, climate_displacement_governance).

% DUAL FORMULATION NOTE:
% This constraint is part of the BORDER_LEGITIMACY kernel family. Three structurally distinct constraints instantiate three readings of the same contested kernel: (1) FREEDOM_OF_MOVEMENT_READING (this file) — borders are illegitimate restrictions on human rights; ε is high; victims include all excluded populations; snare classification dominates. (2) SOVEREIGNTY_READING — state territorial authority is legitimate; borders are a necessary attribute of statehood; ε is low; no victims, coordination benefit; rope/mountain classification. (3) HUMANITARIAN_OBLIGATION_READING — states may admit refugees but not economic migrants; borders are legitimate with humanitarian carve-outs; ε is moderate; victims are non-humanitarian migrants; tangled_rope classification. Each reading has a single, stable ε that does not vary with observer. The readings coexist as incompatible institutional positions held by different coalitions (human-rights bodies, state security apparatus, humanitarian NGOs). The kernel is the text/commitment that all readings draw authority from: the Universal Declaration, state constitutions, international law frameworks. The readings contest what those texts legitimately entail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
