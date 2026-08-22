% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: The Rhetra as Apolline Ordinance — Sacral Fidelity Reading
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the sacral fidelity reading of the Lycurgan-laws
 *   kernel: the Rhetra, delivered under Delphic sanction, is understood by
 *   the Spartan tradition itself as a fixed, divine ordinance, not a human
 *   political settlement subject to revision. Within this reading, Sparta's
 *   stability (eunomia) for several centuries is treated as confirmation of
 *   the ordinance's sacred status, and Sparta's eventual military and
 *   demographic decline (fourth century BCE oliganthropia) is attributed,
 *   from within the reading, to external military pressures (the Theban-led
 *   liberation of Messenia) and to citizen moral failure (accumulation of
 *   private wealth, decline of communal discipline) rather than to any
 *   structural defect in an unrevisable constitution. This is a live
 *   self-understanding recorded by Plutarch and Xenophon and echoed in later
 *   constitutional theory that treats fixed founding constitutions as sources
 *   of stability. It is generated here as its own clean constraint,
 *   structurally distinct from the demographic_trap_reading (which locates
 *   causal responsibility for collapse in the immutability itself) and the
 *   adaptive_fiction_reading (which holds that the 'immutable' law was
 *   covertly revised throughout and the sacred framing was cover for ongoing
 *   adaptation). All three readings share the kernel — the Rhetra and its
 *   claimed status — but diverge on where legitimacy is grounded and what
 *   caused Sparta's fate; per the ε-invariance principle each is authored as
 *   a separate constraint with its own ε, and this file's ε is authored
 *   strictly from the sacral reading's own lights, not averaged against the
 *   siblings.
 *
 * KEY AGENTS:
 *   - spartiate_citizen_body: Primary beneficiary/payer (organized/identity_locked) — lives the discipline, receives the eunomia
 *   - ephorate_and_gerousia: Agenda-setter (institutional/constrained) — enforces but does not claim to originate the law
 *   - delphic_oracle_priesthood: Beneficiary (institutional/arbitrage) — supplies and benefits from the divine warrant
 *   - helot_population: Excluded (powerless/trapped) — labor substrate never addressed by the sacred covenant
 *   - later_historians_and_philosophers: Analytical observer — records but cannot adjudicate the sacral claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.28).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "The Rhetra as Apolline Ordinance — Sacral Fidelity Reading").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'f6488a61-d441-41df-a48f-532ec543d4fa').
narrative_ontology:cs_kernel_codification('f6488a61-d441-41df-a48f-532ec543d4fa', fixed_text).
narrative_ontology:cs_authority_grounding('f6488a61-d441-41df-a48f-532ec543d4fa', lineage).
narrative_ontology:cs_interpretation_layer_present('f6488a61-d441-41df-a48f-532ec543d4fa').
narrative_ontology:cs_reading_relation('f6488a61-d441-41df-a48f-532ec543d4fa', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6488a61-d441-41df-a48f-532ec543d4fa', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('f6488a61-d441-41df-a48f-532ec543d4fa', foundational, rhetra_is_divinely_sanctioned_and_unrevisable).
narrative_ontology:cs_axiom_status(rhetra_is_divinely_sanctioned_and_unrevisable, holdable).
narrative_ontology:cs_axiom_grounding('f6488a61-d441-41df-a48f-532ec543d4fa', rhetra_is_divinely_sanctioned_and_unrevisable, theological).
narrative_ontology:cs_axiom('f6488a61-d441-41df-a48f-532ec543d4fa', foundational, spartan_decline_caused_by_external_pressure_and_citizen_vice_not_system_design).
narrative_ontology:cs_axiom_status(spartan_decline_caused_by_external_pressure_and_citizen_vice_not_system_design, holdable).
narrative_ontology:cs_axiom_grounding('f6488a61-d441-41df-a48f-532ec543d4fa', spartan_decline_caused_by_external_pressure_and_citizen_vice_not_system_design, empirically_contingent).
narrative_ontology:cs_reference_frame('f6488a61-d441-41df-a48f-532ec543d4fa', apolline_sanctioned_rhetra).
narrative_ontology:cs_drift_state('f6488a61-d441-41df-a48f-532ec543d4fa', fourth_century_oliganthropia_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('f6488a61-d441-41df-a48f-532ec543d4fa', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, ephorate_and_gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_sanction_of_lycurgan_order).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, eunomia_as_cosmic_harmony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lives under the agoge and the common messes, understanding citizenship itself as constituted by unwavering obedience to Lycurgus's ordinances. Benefits from the eunomia (good order) the laws are believed to secure, and bears the full weight of the discipline — celibacy penalties, mess contributions, lifelong military service — as the price of the sacred order. Exit would mean ceasing to be Spartan in any meaningful sense; the constraint is not experienced as external coercion but as identity itself.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body, payer).

% Administers and enforces adherence to the ancestral laws, adjudicating disputes and disciplining deviation. Understands its own authority as flowing entirely from fidelity to the received ordinance, not from any independent political mandate — the officials cannot claim to legislate, only to guard what was given.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate_and_gerousia, agenda_setter,
    institutional, generational, constrained, regional).

% The oracle's historical pronouncement sanctifying Lycurgus's Rhetra is the founding warrant for the entire system's sacred status. The priesthood's prestige and continued relevance as arbiter of divine will are affirmed every time Sparta treats the ordinance as beyond human revision.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_priesthood, beneficiary,
    institutional, civilizational, arbitrage, regional).

% Bears the agricultural labor that frees Spartiate men for full-time military and civic life under the Lycurgan order, but has no voice in the sacred-law framework at all — the ordinance's sanctity is asserted entirely by and for the Spartiate citizen body and is never addressed to the helots as participants in any covenant.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, excluded,
    powerless, biographical, trapped, regional).

% Plutarch, Xenophon, and later constitutional theorists record and evaluate the claim that the Rhetra was divine and unchangeable, providing the tradition's own testimony without independent power to confirm or deny the sacral claim.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, later_historians_and_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Sparta a single, uncontestable source of legal and social order — because the laws are of divine origin and were fixed once by Lycurgus under Apollo's sanction, no citizen, official, or assembly can claim standing to alter them, eliminating factional lawmaking and civic strife (stasis) over legislation.
% TRANSFER_FUNCTION: Moves individual discretion and generational adaptive capacity from every citizen and official into permanent deposit with the ancestral ordinance; in return, citizens receive a stable, legitimated social order that requires no ongoing political negotiation.
% ABSENT_VOICES: The helot population, whose forced agricultural labor makes the Spartiate lifestyle materially possible, has no standing whatsoever within the sacral framework — the ordinance addresses only the Spartiate citizen body as covenant participants, and no voice within this reading contemplates helot interests as relevant to the law's sanctity.
% DISAPPEARANCE_RATIONALE: Within this reading, if the Rhetra's sacred status were somehow to lapse, the reading holds that Spartan order itself would rearrange catastrophically — eunomia collapses without divine sanction, per the tradition's own self-understanding. Whether this is true or whether the sacred framing merely masks an ordinary human institution is exactly the question the sibling readings contest; the sacral reading asserts world_rearranges as a matter of cosmic necessity, not empirical prediction.
% FOUNDING_PROBLEM: Archaic Sparta faced civic disorder (stasis) and the need for a stable social, military, and land-tenure order; the Rhetra is presented as Apollo's own remedy, delivered through Lycurgus, to establish eunomia once and for all.
% FOUNDING_PROBLEM_CORROBORATION: The Delphic priesthood and the Spartan state itself attest to the ordinance's continuing sacred force — both are direct beneficiaries of that attestation. Outside corroboration is genuinely thin: Herodotus and Thucydides report Spartan eunomia as a stable external fact but do not themselves endorse the divine-origin claim, and no independent non-Spartan, non-priestly source corroborates the sacral status specifically; this reading's founding-problem status rests substantially on self-testimony.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, contested).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because, from within this reading's own lights, the ordinance is not experienced as extraction at all by its addressed community — the Spartiate body understands the discipline as constitutive of a shared good, not a transfer to an external party. Some low-level extraction is nonetheless authored because the helot population's labor is a real cost the sacred framework never acknowledges as a cost, and because the priesthood and officialdom draw real prestige and authority from the arrangement. Suppression is moderate-high (0.62) and rises slowly, reflecting the genuine coercive machinery (agoge discipline, krypteia against helots, social penalties for deviance) that a claimed-mountain reading of the constitution still requires to maintain the appearance of an unrevisable, uncontested order — a true mountain would need none of this enforcement, and its presence is itself a datum the engine can weigh against the claim. Accessibility collapse is authored very high (0.88): within Spartan civic culture, the alternative of a revisable, ordinary human law is treated as literally unthinkable, which is the signature of a mountain claim (correctly or not). Resistance is authored low (0.15): little organized internal resistance to the sacred framing existed among Spartiates themselves; what resistance existed was external (Thebes, Messenian helots) or emerged much later in historiography. The claim (mountain) and the metrics (non-trivial suppression, non-zero extraction, declared beneficiaries) are authored independently per the framework's rule — the divergence between a claimed natural-law status and a metric profile showing real enforcement and real beneficiaries is exactly the FSM signature this story is built to expose, not a contradiction to be smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartiate citizen body sits near symmetric-to-beneficiary: they bear the discipline but are also the primary intended beneficiaries of the eunomia the ordinance is claimed to secure, and their exit option is identity-locked rather than merely constrained — leaving the system means ceasing to be Spartan, a much stronger binding than economic constraint. The ephorate and gerousia are structurally accountable to the ordinance rather than authors of it, sitting closer to a constrained administrative seat than a full beneficiary seat despite their institutional power. The Delphic priesthood is authored as a clear beneficiary with mobile/arbitrage exit — its authority as diviner of the sacred is portable and does not depend on Sparta's continued existence. The helot population is excluded entirely from the covenant's addressee class; they are not even counted as targets within this reading's own terms, which is itself the reading's most significant blind spot and is flagged via the absent_voices field rather than smuggled into a beneficiary/victim declaration this reading itself would not make.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly treats the founding problem (archaic stasis, need for stable order) as still live and treats zero revision capacity as virtue rather than defect — mandatrophy is, by the reading's own lights, not applicable, because the mandate is sacred and cannot expire. This is precisely the self-sealing feature the demographic_trap_reading and adaptive_fiction_reading each contest from different directions: one holds the mandate did expire (functionally, in demographic terms) while the sacred framing prevented revision; the other holds the mandate was never really fixed at all. This story does not adjudicate between them — it authors the sacral reading's own account faithfully, including its own resistance to the very idea of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacral_status_natural_law_vs_constructed,
    'Is the Rhetra''s ''divine, unchangeable'' status a genuine natural-law-like feature of Spartan constitutional order, or a constructed claim that benefits identifiable agents (the Spartiate citizen body''s self-understanding, the ephorate''s enforcement authority, and especially the Delphic priesthood''s continuing relevance as sanctioning authority)?',
    'Comparative analysis of the historical timeline: does the oracular sanction narrative predate or postdate the political need it serves? Later scholarship (this reading''s tradition itself, via Plutarch quoting earlier sources) versus independent archaeological and epigraphic evidence bearing on the actual dating and content of the Rhetra.',
    'If constructed to serve identifiable beneficiaries (priesthood prestige, elite enforcement authority, in-group cohesion), this constraint is a textbook false-summit-mountain candidate and the engine''s FSM signature should be expected to fire given the declared beneficiaries; if genuinely treated as beyond human agency by all parties without instrumental benefit, the mountain claim is more defensible on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_status_natural_law_vs_constructed, conceptual, 'Natural-law claim vs. constructed sacred authority serving identifiable beneficiaries.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the sacral_fidelity, demographic_trap, and adaptive_fiction readings of the Lycurgan-laws kernel structurally diverge?',
    'The three readings agree that the Rhetra was treated as fixed constitutional bedrock and that Sparta eventually declined; they diverge on (a) authority grounding — divine sanction (sacral) vs. no grounding claim, purely structural (demographic_trap) vs. covert human political engineering under a sacred veneer (adaptive_fiction); and (b) causal attribution for decline — external pressure/citizen vice (sacral) vs. the immutability itself as the causal mechanism (demographic_trap) vs. the immutability being a fiction so the real causes lie elsewhere entirely (adaptive_fiction). Resolving which reading better fits the historical record would require independent (non-Spartan, non-priestly) primary source corroboration of whether the Rhetra was ever actually revised in practice.',
    'If adaptive_fiction is correct, this sacral reading''s zero-extractiveness self-understanding is itself the extraction mechanism (legitimating covert elite adaptation); if demographic_trap is correct, the sacral reading''s virtue-framing of zero revision capacity directly obscures the mechanism of Sparta''s collapse; either resolution would substantially undercut this reading''s mountain claim without changing this file''s own authored ε, which remains fixed to what THIS reading''s lights assert.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the specific structural disagreement between the three kernel readings for downstream Omega_C analysis.').

omega_variable(
    helot_exclusion_scope_ambiguity,
    'Does the sacred covenant''s silence regarding helots represent a genuine category boundary (the ordinance was never intended to address non-citizens) or a suppressed extraction relationship dressed as scope limitation?',
    'Examination of krypteia institutional records and helot revolt frequency as evidence of whether the ''excluded'' framing reflects actual indifference or active, ongoing management of a suppressed population whose exclusion is functionally necessary to the citizen body''s leisure for civic/military life.',
    'If the exclusion is functional suppression rather than mere scope limitation, the helot population should arguably be reclassified as victims rather than merely excluded, which would push this reading toward tangled_rope rather than mountain — but this reading''s own lights do not make that reclassification, which is itself the datum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(helot_exclusion_scope_ambiguity, empirical, 'Whether helot exclusion from the sacred covenant is benign scope-limitation or suppressed extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 160, 0.32).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 240, 0.35).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 320, 0.38).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 400, 0.4).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 160, 0.22).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 240, 0.24).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 320, 0.26).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 400, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 160, 0.59).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 240, 0.6).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 320, 0.61).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
