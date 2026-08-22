% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty and Humanitarian Intervention Authority
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'conditional responsibility' reading of
 *   the Westphalia sovereignty kernel: states forfeit territorial
 *   inviolability when failing to protect populations from mass atrocities.
 *   Under this reading, sovereignty is not an absolute right of
 *   non-interference but a conditional status that depends on meeting
 *   protection obligations. The reading emerged in the late 20th century as a
 *   response to genocides (Rwanda, Bosnia, Darfur) where absolute
 *   non-intervention doctrine provided no mechanism for international
 *   response. It redistributes authority: from individual states (who
 *   determine their own legitimacy) to international institutions and
 *   humanitarian coalitions (who judge sovereignty status). The constraint is
 *   CLAIMED as tangled_rope because it solves a genuine coordination problem
 *   (collective response to atrocity) while simultaneously extracting
 *   authority and legitimacy from atrocity-regime states and traditional
 *   sovereignty defenders. The metrics show high extractiveness (0.68) and
 *   suppression (0.71) because the reading's persistence depends on active
 *   exclusion of rival interpretations and enforcement against states that
 *   resist the judgment of their sovereignty status.
 *
 * KEY AGENTS:
 *   - Atrocity regime state: bears costs of sovereignty conditional on international judgment of protection capacity; structurally trapped.
 *   - Humanitarian intervention coalition: sets adjudicative agenda; benefits from lowered intervention barriers and humanitarian framing; excludes rival powers.
 *   - Global governance institutions (UN, ICC, treaty bodies): gain expanded adjudicative mandate; collect authority as arbiters of sovereignty status.
 *   - Atrocity-affected population: benefits from recognition of atrocity as intervention ground, but bears hidden cost if interventions are poorly calibrated or serve coalition interests over protection.
 *   - Sovereignty protection traditionalists: pay the cost of shifted baseline; identity-locked resistance to the reading.
 *   - Regional rival powers: excluded from coalition membership; exposed to the reading being weaponized.
 *   - International legal scholars: analytical observers measuring whether the reading is applied consistently and achieves stated humanitarian outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.71).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty and Humanitarian Intervention Authority").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2').
narrative_ontology:cs_kernel_codification('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', fixed_text).
narrative_ontology:cs_authority_grounding('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', extraction).
narrative_ontology:cs_interpretation_layer_present('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2').
narrative_ontology:cs_reading_relation('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', foundational, sovereignty_conditional_on_protection_obligation).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', sovereignty_conditional_on_protection_obligation, deontological).
narrative_ontology:cs_axiom('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', foundational, atrocity_forfeits_territorial_immunity).
narrative_ontology:cs_axiom_status(atrocity_forfeits_territorial_immunity, holdable).
narrative_ontology:cs_axiom_grounding('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', atrocity_forfeits_territorial_immunity, deontological).
narrative_ontology:cs_reference_frame('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', conditional_sovereignty_with_international_adjudication).
narrative_ontology:cs_drift_state('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', contemporary_geopolitical_contest, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37fd9e6c-c3ac-407f-a3f8-7f6ccb8c8de2', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, atrocity_prevention_advocacy_networks).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_regime_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, sovereignty_protection_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalition).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, atrocity_affected_population).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_regime_state).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_affected_population).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, weak_or_contested_legitimacy_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A state accused of failing to protect or actively perpetrating mass atrocities against its population. Under the conditional responsibility reading, this state's territorial inviolability is no longer presumptively protected; it becomes subject to external adjudication of its protection capacities and legitimacy. The state bears the cost of being reclassified as a rights violator and becomes exposed to intervention, sanctions, or even forced regime change. Its exit from this status requires demonstrated institutional reform, which may be costly and may not restore sovereignty symmetry.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_regime_state, payer,
    institutional, generational, trapped, global).

% Military and political coalitions (NATO, ad-hoc humanitarian coalitions, regional powers) that claim authority to intervene under the responsibility-to-protect doctrine. They set the adjudicative agenda: which atrocities trigger intervention, what level of evidence suffices, what remedies are proportionate. They benefit from lowered barriers to intervention and gain legitimacy through humanitarian framing. They bear minimal cost if interventions fail or prove counterproductive, as the responsibility is diffused across coalition members and international consensus.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalition, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalition, beneficiary).

% United Nations bodies, International Criminal Court, human rights treaty bodies, and regional judicial institutions that gain expanded authority to adjudicate sovereignty status and intervention legitimacy. These institutions benefit from increased mandate scope and resource allocation. They collect authority and soft power from being positioned as arbiters of when states have forfeited sovereignty protections. They bear no direct enforcement cost (coalitions enforce) and face limited accountability if their adjudications prove erroneous or politically motivated.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, analytical, global).

% Human rights organizations, genocide prevention networks, and advocacy coalitions that gain expanded voice and agenda-setting capacity. The reading treats protection failure as an explicit violation, validating their framing and giving their testimony higher weight in intervention decisions. They benefit from heightened moral authority and institutional access, though they do not directly command enforcement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_prevention_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Populations experiencing or at risk of mass atrocities. They benefit from international recognition of atrocity as grounds for intervention and from lowered barriers to rescue. However, they bear a significant hidden cost: interventions can destabilize further, humanitarian corridors may be weaponized, and their protection becomes dependent on external powers' strategic interests aligning with humanitarian claims. The reading converts them from victims of their state to potential beneficiaries of international authority, but only insofar as intervention serves the coalition's political goals.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_affected_population, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, atrocity_affected_population, payer).

% States, scholars, and international actors committed to absolute non-intervention and Westphalian territorial integrity norms. They bear the cost of a shifted international baseline: states with contested legitimacy or partial capacity lose presumptive protection, and their own domestic authority becomes subject to external judgment. Their exit from this status is identity-locked because rejecting it requires abandoning the institutional identity built on sovereignty symmetry and non-interference. They resist but lack structural power to prevent the reading's institutional embedding.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, sovereignty_protection_traditionalists, payer,
    institutional, generational, identity_locked, global).

% States with weak state capacity, contested legitimacy, or histories of sectarian violence become perpetually vulnerable to intervention claims. Even absent ongoing atrocities, they live under heightened scrutiny. They must maintain compliance with international judgments of their protection capacity, but the standards are set externally and may shift. Their exit options are constrained: they cannot simply refuse international judgment without facing sanctions or intervention, but demonstrating compliance requires sustained institutional investment.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, weak_or_contested_legitimacy_state, payer,
    moderate, generational, constrained, global).

% States that might claim intervention authority for geopolitical reasons but are excluded from coalition membership (e.g., Russia, China when excluded from particular humanitarian coalitions). They are barred from participating in the adjudication of sovereignty status but remain exposed to it being weaponized against them or their allies. Their exclusion from the agenda-setting seats is the enforcement mechanism that sustains the reading's unilateral application.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, regional_rival_power, excluded,
    powerful, generational, trapped, global).

% Analyzes whether the conditional responsibility reading is coherent, whether its evidentiary standards are applied consistently, and whether it produces the humanitarian outcomes it claims. They take testimony from all other seats and measure the constraint's actual operation against its stated justification.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_scholar_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalition).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective response to mass atrocities that previously had no institutional mechanism: codifies when sovereignty presumption is suspended, who has adjudicative authority, and what remedies are legitimate. Solves the coordination problem of multiple states facing atrocity situations: instead of unilateral intervention or inaction, there is a shared standard and process.
% TRANSFER_FUNCTION: Transfers adjudicative authority over sovereignty status from the individual state (who determines its own legitimacy and immunity) to the international community (who judges whether a state has failed protection obligations). Transfers enforcement capacity from the atrocity regime (which controls its borders and military) to humanitarian coalitions (which gain legitimacy and mandate for military or coercive intervention). Transfers protective legitimacy from the domestic state to international institutions that can override domestic authority claims.
% ABSENT_VOICES: Regional powers excluded from coalition membership; non-Western states that experience the reading as a tool for great-power intervention in their affairs; scholars who argue the evidentiary standards are applied inconsistently to Western and non-Western states; atrocity regimes and their allied states; populations whose sovereignty is preserved at the cost of atrocities not being recognized as such under the reading's criteria.
% DISAPPEARANCE_RATIONALE: If conditional responsibility as a legitimating framework vanished, humanitarian coalitions would lose their primary justification for intervention across borders, atrocity regimes would regain presumptive sovereignty immunity, and global governance institutions would lose their expanded adjudicative mandate. The international system would revert toward absolute non-intervention norms or a more decentralized calculation of when unilateral intervention is legitimate. The institutional apparatus built to operationalize the reading—the Responsibility to Protect doctrine, expanded ICC jurisdiction, humanitarian intervention doctrine in international law—would require fundamental re-grounding.
% FOUNDING_PROBLEM: Mid-20th-century recognition that absolute sovereignty protection shields atrocity regimes from external accountability, and that millions dying under their rule creates a moral void in international law. The reading was developed to bridge the gap: if sovereignty is truly conditional on meeting basic protection obligations, then atrocity becomes grounds for intervention rather than grounds for isolation.
% FOUNDING_PROBLEM_CORROBORATION: Genocide prevention scholars and human rights advocates (beneficiary seats) attest the founding problem is live and the reading is necessary. Atrocity regimes and sovereignty traditionalists (payer and excluded seats) attest the founding problem has been over-generalized to justify intervention in internal conflicts that do not rise to atrocity thresholds, or that intervention based on the reading produces worse humanitarian outcomes than inaction. Independent analysis from non-coalition scholars shows inconsistent application: atrocities by coalition members are often overlooked while atrocities by rival powers trigger intervention calls, suggesting the reading operates as a tool for power asymmetry rather than humanitarian consistency.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising through interval 0–25, then plateauing. The rise reflects the reading's institutional embedding: as global governance institutions internalize conditional responsibility into doctrine and procedure, the extraction of adjudicative authority becomes more persistent. Plateau at 25–35 indicates the reading has reached saturation in institutional design; further increases would require explicit doctrine expansion. Suppression is also high (0.71) because the reading's persistence requires active exclusion of the absolute non-intervention reading (which remains held by excluded states and traditionalists). Theater ratio (0.42) indicates the constraint's operation is substantially functional (humanitarian framing, atrocity prevention mechanisms) but with growing performative elements: interventions justified by the reading sometimes serve great-power interests rather than protection outcomes, and the adjudication of sovereignty status is inconsistently applied across coalition and non-coalition states. The measurement grid uses one shared time axis so all metrics are authored at every examined point. The rising extractiveness and suppression through mid-interval reflect the constraint's institutional hardening; the plateau suggests a temporary equilibrium where the reading is embedded enough to be self-reinforcing but faces persistent resistance from excluded seats.
 *
 * PERSPECTIVAL GAP:
 *   From the humanitarian coalition seat: the reading is genuine coordination responding to a moral vacuum; sovereignty should be conditional on protection, and enforcement is necessary to prevent atrocities. From the atrocity regime state and traditionalist seats: the reading is a tool for great-power intervention disguised as universal principle; it legitimates coercive regime change and violates the core principle of equal state dignity. From the atrocity-affected population seat: the reading is necessary but instrumentalized; it provides legitimacy for intervention that sometimes protects and sometimes exploits. From the excluded regional power seat: the reading is selectively applied; coalition-member atrocities are overlooked while rival-power atrocities trigger calls for intervention. The engine computes these divergent experiences from the structural data (power, exit options, role) and stakeholder positions; the authored claim does not adjudicate which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The atrocity regime state occupies the full-target position (d ≈ 0.95): it bears the cost of sovereignty conditionality and has trapped exit options; the international judgment of its status is imposed without its consent or power to resist. Humanitarian coalitions are beneficiaries (d ≈ 0.1): they gain adjudicative authority and legitimacy; their exit options are arbitrage (they can choose which atrocities to highlight and which to ignore based on strategic interest). Global governance institutions are beneficiaries (d ≈ 0.15): they gain expanded mandate and soft power; their analytical exit position insulates them from enforcement costs. Atrocity-affected populations occupy an ambiguous position (d ≈ 0.5–0.6): they benefit from recognition but are trapped in dependence on external powers' decisions about intervention, and the reading can result in interventions that worsen their situation. Sovereignty traditionalists are targets (d ≈ 0.8): they bear the cost of a shifted international baseline and identity-locked exit makes them unable to exit the constraint's scope without abandoning institutional commitments. This per-seat divergence is the engine's computation from the structural data; the claim/metric gap documents it explicitly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atrocity regimes shield their violations behind sovereignty immunity) is CONTESTED in status. Beneficiary seats argue it is still live: recent atrocities (Myanmar, Syria, South Sudan) show that absolute non-intervention produces humanitarian failures. Payer and excluded seats argue it is partially dead or misdescribed: the reading conflates atrocity (which is wrong) with lack of international capacity to prevent it (which is a practical problem, not a sovereign immunity problem). The reading itself can be a vector for corruption of the founding problem: coalitions use the reading to justify interventions that are politically motivated rather than atrocity-responsive, treating the reading as a tool for power assertion rather than humanitarian protection. This corruption is detected by the mismatch: founding_problem_status=contested + disappearance_verdict=world_rearranges + theater_ratio rising to 0.42 suggests the founding problem has partly dissolved (many atrocities are now recognized and discussed internationally, reducing the vacuum the reading solved) but the reading persists for reasons beyond the founding problem (institution inertia, power consolidation, strategic interest in intervention authority). The reading is tangled_rope, not rope, because the coordination benefit (collective response to atrocity) is genuine, but the extraction (authority over sovereignty status, asymmetric intervention powers) is also real and not a necessary cost of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_ambiguity,
    'What threshold of atrocity severity and imminence triggers the loss of sovereignty presumption? Is the threshold objective (e.g., death toll, verified genocide findings) or subjective (adjudicated by coalition judgment)?',
    'Comparison of intervention decisions across multiple cases: if similar death tolls and circumstances produce different intervention responses depending on the state''s alignment with the coalition, the threshold is subjective; if similar responses occur regardless of alignment, the threshold is more objective.',
    'An objective threshold would support the reading''s humanitarian legitimacy; a subjective threshold would establish the reading as a tool for power asymmetry. If subjective, the constraint should reclassify toward snare (pure extraction) rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_ambiguity, empirical, 'Whether atrocity-triggering-intervention thresholds are applied consistently or depend on geopolitical interest.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the conditional responsibility reading logically foreclose the absolute non-intervention reading, or do they coexist as competing frameworks?',
    'Examination of whether a state can coherently hold both readings in its own institutional framework. If a state treats some atrocities as grounds for intervention while defending non-interference in other states'' internal affairs, it is operating both readings simultaneously; if a state must choose one or the other, they foreclose.',
    'If readings foreclose: the contest is zero-sum; one reading will dominate and the other will be institutional casualties. If they coexist: the contest is permanent, and the constraint is a contested temporary equilibrium rather than a settled institutional position. This affects the terminal classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the conditional responsibility reading and absolute non-intervention reading are logically incompatible or operationally coexistent.').

omega_variable(
    coalition_membership_as_enforcement,
    'Is the constraint enforced by the internal logic of conditional responsibility, or by exclusion of rival powers from adjudicative authority?',
    'Counterfactual: if a coalition-member state committed an atrocity that met the reading''s own threshold criteria, would it be subject to intervention under its own principle? If not, the constraint is enforced by power asymmetry; if yes, the constraint is enforced by principle.',
    'If power-asymmetric: the reading is a tool for hegemon consolidation and should reclassify toward snare. If principle-driven: it is genuine tangled_rope. The historical record shows coalition-member atrocities are rarely subjected to intervention calls based on the reading, suggesting power asymmetry, but the reading itself contains no exemption clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_membership_as_enforcement, empirical, 'Whether the conditional responsibility reading is applied to coalition members according to the same standard as non-coalition states.').

omega_variable(
    institutional_capture_of_adjudication,
    'Have global governance institutions (ICC, UN bodies) become captured by the coalitions that set the intervention agenda, such that their adjudications reflect coalition interest rather than neutral judgment of sovereignty status?',
    'Analysis of prosecution patterns, investigation budgets, and remedial recommendations by institution: if these cluster around coalition-aligned interests, capture is probable; if distributed across geopolitical fault lines, capture is less evident.',
    'If captured: the reading''s coordination function is theatricalized; the institutional beneficiaries (governance bodies) are no longer neutral arbiters but coalition tools. The constraint reclassifies toward snare. If independent: the institutions retain legitimacy to judge sovereignty status, and the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_adjudication, empirical, 'Whether international institutions adjudicating sovereignty status and intervention legitimacy remain independent from coalition influence.').

omega_variable(
    reading_vs_sibling_foreclosure_empirical,
    'Has the conditional responsibility reading empirically foreclosed the absolute non-intervention reading at the institutional level, such that non-intervention is no longer a live option in international law?',
    'Examination of UN voting patterns, treaty language, and doctrine development: if the UN Charter''s Article 2.7 (non-interference) is effectively superseded by Responsibility to Protect doctrine, foreclosure is substantive; if both remain operative and invoked contextually, coexistence persists.',
    'Foreclosure would establish the reading as dominant and predict its long-term institutional stability. Coexistence would predict continued contestation and vulnerability to doctrinal reversal if political coalitions shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure_empirical, empirical, 'Whether institutional behavior reflects the conditional responsibility reading as operative doctrine or as a contested alternative to continuing non-intervention norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(west_tr_t5, observed).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(west_tr_t15, observed).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__conditional_responsibility, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(west_tr_t25, observed).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__conditional_responsibility, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(west_tr_t30, observed).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__conditional_responsibility, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(west_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(west_be_t5, observed).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(west_be_t15, observed).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(west_be_t25, observed).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(west_be_t30, observed).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(west_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(west_su_t5, observed).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(west_su_t15, observed).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(west_su_t25, observed).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(west_su_t30, observed).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(west_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% The westphalia_sovereignty kernel is instantiated by three distinct constraint stories, each a reading with different structural properties. This story (conditional_responsibility) treats sovereignty as forfeitable upon protection failure. The absolute_non_intervention reading treats sovereignty as categorical and immune to external judgment. The graded_sovereignty reading treats sovereignty as scalar, calibrated to state capacity. Each reading instantiates different ε, beneficiary sets, and enforcement mechanisms. The three readings coexist in contemporary international law but are in structural tension: a state cannot simultaneously hold that sovereignty is absolutely immune and that it is conditionally forfeitable based on the same set of facts. The readings influence one another: if the conditional reading becomes institutionalized (as the Responsibility to Protect doctrine), it exerts downstream pressure on the absolute reading, raising the cost of holding the non-intervention position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
