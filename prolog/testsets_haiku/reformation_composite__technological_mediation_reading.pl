% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Reformation Enabler (Technological Mediation Reading)
 *   domain: historical/technological/epistemological
 *
 * SUMMARY:
 *   This constraint story instantiates the TECHNOLOGICAL MEDIATION READING of
 *   the Reformation kernel: the core claim is that the printing press is the
 *   primary causal mechanism transforming local theological dissent into
 *   continental mass movement. This reading does NOT argue theology is
 *   unimportant or that political factors are absent. Rather, it establishes
 *   printing technology (and the literacy it enabled) as the enabling
 *   infrastructure without which the scale and speed of reformation is
 *   inexplicable. The printing press is treated as a mountain—a
 *   physical/technological constraint emerging naturally from the logic of
 *   mechanical reproduction—that enables all downstream theological and
 *   political readings. Publication rates and literacy expansion are the
 *   primary observables that distinguish this reading from sibling readings.
 *   The theological fragmentation reading emphasizes incompatible doctrinal
 *   commitments; the political realignment reading emphasizes sovereignty and
 *   imperial contestation; this reading emphasizes the material substrate of
 *   information distribution that made continental coordination of any kind
 *   possible.
 *
 * KEY AGENTS:
 *   - printing_technology: The enabling infrastructure (non-agent); the physical constraint of reproducible text.
 *   - pre_reformation_clergy: Prior monopoly holders on scriptural interpretation; their institutional power depended on manuscript scarcity.
 *   - early_reformers: Local dissenters (Wycliffe, Hus, Luther) whose dissent became continental only via printing scale.
 *   - literate_merchants_and_craftspeople: Primary beneficiaries; printing enabled their participation in theological debate.
 *   - papacy_and_catholic_hierarchy: Payers; lost epistemic monopoly to technology; responded with suppression attempts (Inquisition, Index) that proved the technology was the threat, not the theology alone.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.18).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.12).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Reformation Enabler (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical/technological/epistemological").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'dd32000f-eae6-428b-ab37-1f4cd20888ef').
narrative_ontology:cs_kernel_codification('dd32000f-eae6-428b-ab37-1f4cd20888ef', distributed).
narrative_ontology:cs_authority_grounding('dd32000f-eae6-428b-ab37-1f4cd20888ef', distributed).
narrative_ontology:cs_reading_relation('dd32000f-eae6-428b-ab37-1f4cd20888ef', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('dd32000f-eae6-428b-ab37-1f4cd20888ef', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('dd32000f-eae6-428b-ab37-1f4cd20888ef', foundational, printing_press_as_primary_enabler).
narrative_ontology:cs_axiom_status(printing_press_as_primary_enabler, holdable).
narrative_ontology:cs_axiom_grounding('dd32000f-eae6-428b-ab37-1f4cd20888ef', printing_press_as_primary_enabler, empirically_contingent).
narrative_ontology:cs_axiom('dd32000f-eae6-428b-ab37-1f4cd20888ef', foundational, information_decentralization_inevitable_with_scale).
narrative_ontology:cs_axiom_status(information_decentralization_inevitable_with_scale, holdable).
narrative_ontology:cs_axiom_grounding('dd32000f-eae6-428b-ab37-1f4cd20888ef', information_decentralization_inevitable_with_scale, instrumental).
narrative_ontology:cs_reference_frame('dd32000f-eae6-428b-ab37-1f4cd20888ef', manuscript_scarcity_epistemic_monopoly).
narrative_ontology:cs_drift_state('dd32000f-eae6-428b-ab37-1f4cd20888ef', post_printing_press_proliferation_1560, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('dd32000f-eae6-428b-ab37-1f4cd20888ef', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, intellectual_decentralization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_merchants_and_craftspeople).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, early_reformers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, papacy_and_catholic_institutional_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical substrate enabling rapid reproduction and distribution of written theological claims. Not an agent, but the enabling infrastructure this reading treats as primary causal mechanism. Without printing press capacity, theological dissent remained localized; with it, dissent becomes scalable continental phenomenon.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_technology, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, printing_technology).

% Controlled theological discourse through manuscript scarcity and institutional gatekeeping. Literacy was concentrated; copying was expensive; distribution was hierarchical. The printing press undermined their monopoly on scriptural interpretation not through theological refutation but through technological disruption of information scarcity.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, pre_reformation_clergy, agenda_setter,
    institutional, generational, mobile, universal).

% Initially local theological dissenters (Wycliffe, Hus, Luther) operating within the previous epistemic regime of manuscript circulation and institutional control. Their dissent was not novel; what changed was the technological substrate for scaling it. They bore personal risk (excommunication, execution) but the press transformed local heresy into continental reformation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, early_reformers, payer,
    moderate, biographical, identity_locked, regional).

% Growing urban literacy enabled by printing technology (vernacular Bibles, pamphlets, doctrinal treatises). They could read, debate, and spread theological ideas through networks of correspondence and publication. The printing press made theological participation possible for non-clergy; the constraint enables their intellectual enfranchisement.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_merchants_and_craftspeople, beneficiary,
    organized, biographical, mobile, regional).

% Lost epistemic monopoly to the printing press. Once printing enabled mass-market theological texts, the institutional monopoly on scriptural authority became enforceably breached. Counter-reformation responses (Inquisition, Index, Trent) were attempts to suppress the technological enabler through coercion—evidence that the constraint operated through technology, not theology alone.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, papacy_and_catholic_institutional_hierarchy, payer,
    institutional, generational, constrained, universal).

% The rise of reading ability correlates with and is causally enabled by printing press availability. Literacy rates in Protestant regions exceeded Catholic regions post-1520 because the reformed movement incentivized Bible reading and printing enabled vernacular supply. Literacy is the observable that demonstrates the technological mediation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literacy_expansion, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, literacy_expansion).

% Analytical seat. Argue that the printing press is the primary variable explaining reformation scale and geography: reformation succeeded where printing presses and literacy penetrated, failed where they did not. They examine publication rates, printing center locations, and pamphlet distribution networks as evidence.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformation_historians_technological_school, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solves a technological coordination problem: how to enable simultaneous theological debate across multiple dispersed centers when manuscript-based copying cannot scale. Once printing exists, theological ideas can reach continental audiences in weeks rather than decades, enabling rapid debate and coalition formation.
% TRANSFER_FUNCTION: Transfers epistemic authority from the institutional clergy (who controlled manuscript production and distribution) to distributed actors (reformers, printers, literate merchants, craftspeople) who could now produce, copy, and circulate theological texts at scale. The mechanism: printing technology makes it technologically and economically impossible for any single institution to monopolize scriptural interpretation.
% ABSENT_VOICES: Nonliterate populations and regions without printing press access had no voice in the reformation even where theological dissent existed. Peasant grievances (German Peasants' War) arose from reformation ideas but the peasants themselves could not participate in the textual theological debates—they were enlisted by others who could read and write. The reformation is fundamentally a phenomenon of literate, print-enabled populations.
% DISAPPEARANCE_RATIONALE: If the printing press had not been invented or had remained marginal, theological dissent would have continued (local heresies emerged throughout the medieval period) but would not have become a continental mass movement. The Reformation as a historical phenomenon—continental scale, rapid spread, institutional rupture—disappears without printing. Dissent persists in a different form: localized, slower, more containable by institutional force.
% FOUNDING_PROBLEM: How can theological dissent scale beyond local persecution? How can alternative biblical interpretations reach enough readers to challenge institutional monopoly on scriptural authority? How can reformers' ideas coordinate a continental movement across dispersed populations who cannot meet in person?
% FOUNDING_PROBLEM_CORROBORATION: Historians of printing (Eisenstein, Pettegree) and historians of literacy (Graff) attest from outside the theological tradition: printing and literacy rates correlate causally with reformation geography and speed. Catholic-dominated regions without printing press density resisted reformation longer. This observation comes from empirical historiography independent of theological debate. Theological historians dispute whether theology or technology was primary; technological historians argue the medium was the message.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.18 at interval end) because printing technology does NOT extract in the classical sense—it decentralizes epistemic authority to whoever can access the press and read. The constraint ENABLES intellectual decentralization; it does not concentrate value upward. Suppression is low (0.12 at interval end) because the printing press is a physical reality that cannot be suppressed as easily as theological ideas can be—the Catholic response (Inquisition, book burning, Index) is an attempt to suppress the technology's OUTPUT (ideas), not the technology itself, which persists and proliferates. Theater is very low (0.05) because the printing press has no performative component; it simply works—texts are produced, distributed, read. The measurement trajectory shows suppression RISING over the interval (1440–1560) as the institutional response intensifies: by 1520 the Catholic hierarchy understood the threat and attempted coordinated suppression. Extractiveness rises slightly as printers and early publishers begin to capture rents on religious texts (printing was a for-profit enterprise), but the reading's claim is that extraction is NOT the mechanism—enablement is. The accessibility collapse is very high (0.92) because once printing exists, alternatives to centralized control of scriptural interpretation literally do not exist; the technology makes decentralization inevitable. Resistance is very low (0.08) because the technology itself meets almost no resistance—resistance comes to the IDEAS circulated via the technology, but not to printing as such. Even the Catholic hierarchy did not reject printing; they condemned heretical books and tried to control the press, but accepted printing itself as inevitable.
 *
 * PERSPECTIVAL GAP:
 *   The institutional clergy's seat experiences the printing press as catastrophic loss of epistemic monopoly (high extractiveness from their perspective, high suppression requirement to restore control). The reformers' and merchants' seats experience it as intellectual liberation (low extractiveness, high benefit). The technology itself has no perspective—it is a mountain. The engine should compute the technological reading as a genuine mountain from the analytical seat because the accessibility collapse (0.92) and low resistance (0.08) are consistent with a physical/technological constraint. From the clergy's institutional seat, the constraint might compute as tangled_rope or snare depending on how one attributes the loss of monopoly—was it extraction (the printing press was designed to undermine clerical authority) or coordination (printing enabled the coordination of dissent that was always latent in theological disagreement)? The reading itself claims mountain; the divergence in per-seat computation is exactly what the engine should measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press as a physical technology does not extract in the manner of a classical snare or tangled rope. It ENABLES others to extract themselves from centralized control. The 'intellectual_decentralization' beneficiary is not an agent (it is a non-agent entry in the stakeholders array, agent=false) because the benefit is abstract—it is the opening of possibility, not the concentration of gains. The pre-reformation clergy are payers in the sense that they LOSE monopoly position, but they do not pay in the extractive sense (money/labor flowing upward); they lose power. This directionality is primarily asymmetric in the temporal dimension: early reformers (biographical time horizon) gain means to coordinate their message, while the clergy (generational time horizon, institutional entrenchment) face escalating coordination problems. No directionality override is needed because the derivation chain produces the right result: beneficiaries (intellectual decentralization, literate merchants) get low d (they gain access); payers/clergy get moderate d (they lose institutional gatekeeping power but are not personally trapped). The printing press itself gets d=0.5 (analytical, neither target nor beneficiary—it is the neutral enabler).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by fixing on the technological substrate rather than the theological mandate. If the founding problem were stated as 'how to convince Christians of reformed doctrine,' the constraint would be theology and the founding problem might be resolved (if all became reformed or all remained Catholic). If the founding problem were 'how to weaken papal authority,' it might be resolved (the papacy adapted, the Counter-Reformation stabilized). But the founding problem in this reading is 'how to enable decentralized theological debate at continental scale'—a problem native to the printing technology itself, not to its theological or political instantiation. Printing enabled decentralized debate; whether that debate resolved into denominations (theological reading) or national churches (political reading) is downstream. The constraint persists because the printing press persists. The founding problem remains live: printing continues to enable decentralized information circulation, and this remains structurally inimical to centralized institutional control of truth claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    printing_as_mountain_or_technological_snare,
    'Is the printing press a genuine natural constraint (mountain) enabling decentralization, or is the framing of printing as ''neutral enabler'' itself an ideology that obscures the strategic decisions about who owned presses, who could afford to print, and whose views got circulated?',
    'Examine the actual distribution and ownership of printing presses in the sixteenth century; determine whether access was open or controlled; analyze whether printing was used strategically to suppress certain theological or political voices as often as to liberate dissent.',
    'If printing was genuinely open and neutral, the mountain classification stands. If printing access was concentrated and strategic, the constraint becomes tangled_rope or snare: technological enablement as cover story for who actually controlled publication and dissemination. This would demote the reading from a claim about printing-as-such to a claim about the specific politics of early modern print capitalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_as_mountain_or_technological_snare, empirical, 'Whether printing technology was inherently decentralizing or whether decentralization rhetoric masked print industry concentration.').

omega_variable(
    literacy_as_endogenous_or_exogenous,
    'Did printing cause the rise in literacy, or did rising literacy cause the demand for printing? Is literacy the primary observable supporting the technological reading, or is it endogenous to broader social and economic changes (urbanization, merchant capitalism, educational demand)?',
    'Examine whether literacy rates rose before or after printing press availability in different regions; separate causality from correlation.',
    'If literacy rises BEFORE printing (exogenous), then printing amplifies existing demand for texts, and the constraint is genuinely enabling (mountain). If printing CAUSES literacy rise, then printing is a transformative technology and the mountain classification is stronger. If literacy and printing co-evolve with no clear causal direction, the claim about printing as primary variable weakens, and the technological reading becomes one factor among several (tangled with political/theological factors rather than primary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_as_endogenous_or_exogenous, empirical, 'Causal direction between literacy and printing technology.').

omega_variable(
    theological_dissent_without_printing,
    'The Reformation reading emphasizes printing as enabler. But theological dissent existed throughout the medieval period (Waldensians, Lollards, Hussites, Wycliffe). Why did these earlier movements not produce a continental reformation? Was it printing that was missing, or something else (coercive institutional response, lack of political opening, absence of charismatic leaders, different theological framing)?',
    'Comparative historical analysis: examine why earlier heresies were suppressed and contained, while the sixteenth-century reformation succeeded despite suppression. Isolate the role of printing from the role of political fragmentation (nation-states emerging as power centers), of Luther''s specific theological framing, and of the accidental opening created by the Diet of Worms and Charles V''s distraction.',
    'If printing was merely a necessary enabler and political/theological factors were sufficient, the technological reading weakens and the political/theological readings become co-equal or primary. If printing was the genuine game-changer—earlier heresies failed to scale despite theological similarity because printing infrastructure did not exist—the technological reading is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_dissent_without_printing, conceptual, 'Whether printing was the limiting factor preventing earlier heresies from achieving reformation scale.').

omega_variable(
    reading_contest_boundary,
    'This reading treats printing as a MOUNTAIN (natural, enabling, enabling rather than extracting). But the political and theological readings treat printing as a context—as one variable among several. Is printing a primary constraint, or is it one element of a larger system in which politics and theology are equally causal? If the readings truly compete, what observation would falsify the technological reading in favor of the others?',
    'Define a measurement that would distinguish: if printing rates correlate with reformation geography better than political/theological factors do, the technological reading wins; if political and theological factors predict reformation outcomes independently of printing availability, the readings coexist (each capturing part of the variance). The kernel contest is not resolved by evidence; it is resolved by which framing is more productive for understanding the historical phenomenon.',
    'This omega documents that the three readings do not exhaust explanatory variance—they compete for narrative primacy, not for empirical uniqueness. The engine should classify this as a contingency: the technological reading is a valid mountain-framing only if printing is treated as primary. Once theological or political factors enter the picture as co-equal, the decomposition breaks down and the three readings should be integrated into a single tangled_rope story with multiple mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_boundary, conceptual, 'Whether the technological reading is genuinely primary or one interpretation among coequal alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1440, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1440, reformation_composite__technological_mediation_reading, theater_ratio, 1440, 0.0).
narrative_ontology:measurement_basis(refo_tr_t1440, projected).
narrative_ontology:measurement(refo_tr_t1460, reformation_composite__technological_mediation_reading, theater_ratio, 1460, 0.02).
narrative_ontology:measurement_basis(refo_tr_t1460, observed).
narrative_ontology:measurement(refo_tr_t1480, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.03).
narrative_ontology:measurement_basis(refo_tr_t1480, observed).
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__technological_mediation_reading, theater_ratio, 1500, 0.04).
narrative_ontology:measurement_basis(refo_tr_t1500, observed).
narrative_ontology:measurement(refo_tr_t1520, reformation_composite__technological_mediation_reading, theater_ratio, 1520, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1520, observed).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__technological_mediation_reading, theater_ratio, 1540, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1540, observed).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__technological_mediation_reading, theater_ratio, 1560, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1560, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1440, reformation_composite__technological_mediation_reading, base_extractiveness, 1440, 0.0).
narrative_ontology:measurement_basis(refo_be_t1440, projected).
narrative_ontology:measurement(refo_be_t1460, reformation_composite__technological_mediation_reading, base_extractiveness, 1460, 0.05).
narrative_ontology:measurement_basis(refo_be_t1460, observed).
narrative_ontology:measurement(refo_be_t1480, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement_basis(refo_be_t1480, observed).
narrative_ontology:measurement(refo_be_t1500, reformation_composite__technological_mediation_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement_basis(refo_be_t1500, observed).
narrative_ontology:measurement(refo_be_t1520, reformation_composite__technological_mediation_reading, base_extractiveness, 1520, 0.15).
narrative_ontology:measurement_basis(refo_be_t1520, observed).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__technological_mediation_reading, base_extractiveness, 1540, 0.18).
narrative_ontology:measurement_basis(refo_be_t1540, observed).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__technological_mediation_reading, base_extractiveness, 1560, 0.18).
narrative_ontology:measurement_basis(refo_be_t1560, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1440, reformation_composite__technological_mediation_reading, suppression_requirement, 1440, 0.0).
narrative_ontology:measurement_basis(refo_su_t1440, projected).
narrative_ontology:measurement(refo_su_t1460, reformation_composite__technological_mediation_reading, suppression_requirement, 1460, 0.03).
narrative_ontology:measurement_basis(refo_su_t1460, observed).
narrative_ontology:measurement(refo_su_t1480, reformation_composite__technological_mediation_reading, suppression_requirement, 1480, 0.05).
narrative_ontology:measurement_basis(refo_su_t1480, observed).
narrative_ontology:measurement(refo_su_t1500, reformation_composite__technological_mediation_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement_basis(refo_su_t1500, observed).
narrative_ontology:measurement(refo_su_t1520, reformation_composite__technological_mediation_reading, suppression_requirement, 1520, 0.11).
narrative_ontology:measurement_basis(refo_su_t1520, observed).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__technological_mediation_reading, suppression_requirement, 1540, 0.12).
narrative_ontology:measurement_basis(refo_su_t1540, observed).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__technological_mediation_reading, suppression_requirement, 1560, 0.12).
narrative_ontology:measurement_basis(refo_su_t1560, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.05).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% The Reformation kernel decomposes into three readings, each a separate constraint with distinct ε values and stakeholder structures. The technological_mediation_reading treats printing and literacy as primary observables and models the printing press as an enabling mountain constraint. The theological_fragmentation_reading treats doctrinal incompatibilities as primary and models theology as an extractive tangled_rope (contested claims on Biblical authority). The political_realignment_reading treats sovereignty and territorial differentiation as primary and models religious differentiation as an instrument of statecraft (snare or tangled_rope depending on the institutional seat). All three are linked via network.affects_constraints; the printing press (technological reading) creates the material substrate enabling both theological debate and political coalitioning (downstream to both sibling readings). The theological and political readings do not logically foreclose the technological reading; they coexist as different framings of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
