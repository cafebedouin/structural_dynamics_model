% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__contingent_thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__contingent_thinkability_reading, []).

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
 *   constraint_id: zero_as_number_entry__contingent_thinkability_reading
 *   human_readable: Framework-Gated Thinkability of Zero-as-Number in the European Lineage (Contingent-Transmission Reading)
 *   domain: history of mathematics/philosophy of mathematics/conceptual history
 *
 * SUMMARY:
 *   In the European lineage, the concept of zero as an operable number had no
 *   permitted place for roughly fifteen centuries: the Greek settlement that
 *   grounded number in plurality of units and reserved quantity-talk for
 *   magnitudes made the negation of quantity unquantifiable, and the
 *   scholastic institutions that administered that settlement kept it
 *   teachable and everything else unteachable. The concept entered Europe
 *   only through the Sanskrit- and Arabic-language mathematical traditions
 *   that had made zero a number centuries earlier, carried by translators and
 *   merchant networks, resisted by municipal bans, and finally absorbed so
 *   thoroughly that the receiving tradition's own histories recounted the
 *   episode largely as its own acquisition. This story assesses that standing
 *   arrangement — framework-gated thinkability, transmission-dependent
 *   possession, and the credit settlement that followed — by the
 *   contingent-thinkability reading's own lights: the barrier was real,
 *   constructed, contingently absent elsewhere, and the possession it yielded
 *   was received, not generated. KEY AGENTS (by structural relationship): -
 *   european_mathematical_tradition: Primary target and residual collector
 *   (institutional/identity_locked) — bears the dependency admission;
 *   collected the credit stream - non_western_knowledge_systems: Declared
 *   creditor (moderate/trapped) — generated the concept; owed priority
 *   recognition; bore the erasure - scholastic_aristotelian_authorities:
 *   Historical administrator (institutional/constrained) — enforced the
 *   ontology; collected barrier-era rents -
 *   medieval_computists_and_merchants: Frontline bearers
 *   (moderate/constrained) — paid the computational cost; resisted via
 *   adoption - transmission_agents: Channel operators (moderate/mobile) —
 *   moved the texts; collected standing and patronage -
 *   eurocentric_historiography: Narrative administrator
 *   (institutional/identity_locked) — settled the credit question in the
 *   receiver's favor - comparative_history_of_science: Analytical observer
 *   (analytical/analytical) — sees the full transmission structure. This
 *   story is one member of a decomposed family; see
 *   network.dual_formulation_note and commentary.kernel_context for the
 *   family relations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, 0.71).
domain_priors:suppression_score(zero_as_number_entry__contingent_thinkability_reading, 0.52).
domain_priors:theater_ratio(zero_as_number_entry__contingent_thinkability_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__contingent_thinkability_reading, tangled_rope).
narrative_ontology:human_readable(zero_as_number_entry__contingent_thinkability_reading, "Framework-Gated Thinkability of Zero-as-Number in the European Lineage (Contingent-Transmission Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__contingent_thinkability_reading, "history of mathematics/philosophy of mathematics/conceptual history").

domain_priors:requires_active_enforcement(zero_as_number_entry__contingent_thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__contingent_thinkability_reading, 'e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38').
narrative_ontology:cs_kernel_codification('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', distributed).
narrative_ontology:cs_authority_grounding('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', expertise).
narrative_ontology:cs_interpretation_layer_present('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38').
narrative_ontology:cs_reading_relation('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', zero_as_number_entry__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', foundational, thinkability_requires_framework_permission).
narrative_ontology:cs_axiom_status(thinkability_requires_framework_permission, holdable).
narrative_ontology:cs_axiom_grounding('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', thinkability_requires_framework_permission, empirically_contingent).
narrative_ontology:cs_axiom('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', foundational, reception_creates_recognition_debt).
narrative_ontology:cs_axiom_status(reception_creates_recognition_debt, holdable).
narrative_ontology:cs_axiom_grounding('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', reception_creates_recognition_debt, deontological).
narrative_ontology:cs_reference_frame('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', transmission_dependent_thinkability).
narrative_ontology:cs_drift_state('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', comparative_decolonial_historiography_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('e6b3cd45-c8c6-43fc-9b5d-15b4c2b54b38', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, non_western_knowledge_systems).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, scholastic_aristotelian_authorities).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, medieval_computists_and_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, transmission_agents).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, non_western_knowledge_systems).
narrative_ontology:constraint_vindicates(zero_as_number_entry__contingent_thinkability_reading, aristotelian_unit_plurality_doctrine).
narrative_ontology:constraint_vindicates(zero_as_number_entry__contingent_thinkability_reading, number_magnitude_dichotomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Sanskrit- and Arabic-language mathematical traditions that developed positional decimal notation and treated zero as a number operable inside calculation — Brahmagupta's rules for computing with zero, the Arabic algebraic and algorithmic corpus. Their work crossed into Europe through translation and trade. What flowed back for centuries was silence: the receiving tradition's accounts of arithmetic's history rarely named them, and the priority their work earned went unrecorded where the credit question was settled. They hold no seat in the institutions that settled it; their contribution is already given and cannot be withdrawn.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, non_western_knowledge_systems, beneficiary,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, non_western_knowledge_systems, payer).

% The Latin-European lineage of mathematics from the medieval universities through the early modern practitioners to the modern discipline. For roughly fifteen centuries its inherited account of what a number is offered no place for zero as a quantity, and its computational practice ran on Roman numerals and reckoning boards. When positional arithmetic arrived from Arabic-language sources it adopted the methods, built algebra and analysis on them, and thereafter recounted the episode largely as its own acquisition. Its self-understanding as an autonomous rational enterprise is bound up with that recounting; conceding full dependence on transmitted inputs sits uneasily with the self-image, which is what makes the accounting hard to revise from inside.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition, payer,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition, beneficiary).

% The university faculties and ecclesiastical authorities that administered the Latin curriculum and decided which accounts of number, quantity, and the infinite were teachable. They maintained the division between discrete number and continuous magnitude under which the negation of quantity could not itself be a quantity. Control over curricula, licenses, and orthodoxy gave them practical power to admit or refuse new computational methods; the municipal prohibitions on Arabic numerals in commercial records were issued and policed from seats like theirs. Their authority was constituted through the framework, so they could reinterpret it at the margins but not abandon it without dissolving the authority itself.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, scholastic_aristotelian_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% The reckoners, customs officials, and trading houses of Mediterranean Europe who needed fast, reliable arithmetic. Roman-numeral bookkeeping and board reckoning served but scaled poorly with commercial volume; the transmitted positional methods were faster and less error-prone. Several cities banned the numerals in commercial records on fraud-prevention grounds — a zero can be turned into a six or nine by a stroke — pushing the new methods into parallel or clandestine use. Adoption proceeded anyway, market by market, over roughly two centuries, carried by people with no seat in the institutions setting the rules.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, medieval_computists_and_merchants, payer,
    moderate, biographical, constrained, regional).

% The translators, copyists, and scholar-travelers in the line of the Toledo translation school, Gerard of Cremona, and Fibonacci, who carried Arabic-language mathematical texts into Latin Europe and adapted them for Latin readers. They collected standing, patronage, and commercial advantage from operating the channel, and their choices of what to translate and how to gloss it determined what the receiving tradition would see of the source traditions.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, transmission_agents, beneficiary,
    moderate, biographical, mobile, continental).

% The nineteenth- and twentieth-century academic discipline that wrote the standard histories of mathematics. Working largely from Greek and Latin sources, it narrated arithmetic's development as a European achievement with Oriental preliminaries, settling the credit question in the receiving tradition's favor. Its source base, language training, and institutional location made the asymmetry self-reproducing: the scholars trained to check the account were trained inside it. Recent comparative scholarship has begun correcting the record from within the discipline.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, eurocentric_historiography, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, eurocentric_historiography, observer).

% The living scholarly communities of the Sanskrit and Arabic mathematical corpora — philologists and historians of science in India, the Middle East, and their diasporas. Their testimony on dating, attribution, and the operational status of zero in the source texts was not sought when the standard accounts were written, and they remain peripheral to the curricula and reference works that carry the received narrative. They would date and attribute the operative concept differently than the accounts they were excluded from shaping.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, source_tradition_scholarly_communities, excluded,
    moderate, generational, trapped, global).

% Late twentieth- and twenty-first-century scholars working across Sanskrit, Arabic, and Latin sources simultaneously. They reconstruct the transmission record, test the framework-barrier hypothesis against Byzantine and Latin cases, and publish the corrective attributions. They collect no rents from the arrangement and bear none of its costs; their seat is the analytical vantage from which the full structure is visible.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, comparative_history_of_science, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
narrative_ontology:fixing_cost_class(zero_as_number_entry__contingent_thinkability_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Greek settlement coordinated European deductive practice: a shared ontology of number (plurality of units) and magnitude (continuous quantity) let geometers, logicians, and later university faculties mean the same thing by 'quantity,' licensing proof-based mathematics and a standardized curriculum across Latin Europe.
% TRANSFER_FUNCTION: Three flows, in sequence: the framework withheld number-status from zero, costing computists efficiency for centuries; transmission then moved the concept and its notation from Indian and Arabic mathematics into Europe; historiographical practice afterward moved credit for the concept toward the European receivers and away from the generators.
% ABSENT_VOICES: The Sanskrit- and Arabic-source scholarly communities had no seat when the credit question was settled: the standard histories were written from Greek and Latin sources by scholars trained inside the receiving tradition, and the custodians of the generator traditions were neither consulted nor translated into the conversation. They would attribute the operative concept centuries earlier and outside Europe entirely.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if the framework had permitted operand-zero indigenously, or if the credit settlement had honored the transmission record — European arithmetic would have matured centuries earlier on a different schedule, the algebraic revolution would have had different raw material, and the historiography of mathematics would name Brahmagupta and al-Khwarizmi as principals rather than preliminaries. Curricula, reference works, and the receiving tradition's self-account would all rearrange.
% FOUNDING_PROBLEM: The framework was built to secure deductive rigor: Greek mathematics faced the Eleatic paradoxes and the incommensurability crisis, and the settlement that grounded number in plurality of units and barred zero-as-quantity was the price of keeping proof free of paradox.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of Greek mathematics trace the number/magnitude settlement to the rigor crisis (attesting the barrier's origin as a solution to a real problem, not a rent design), and Indologists and historians of Arabic science document the independent Indian and Islamic treatment of zero as an operable number centuries before European adoption. No corroboration exists for the receiving tradition's self-account of autonomous discovery — that account is asserted only from within the benefiting parties, which is itself signal.
narrative_ontology:disappearance_verdict(zero_as_number_entry__contingent_thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__contingent_thinkability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__contingent_thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__contingent_thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__contingent_thinkability_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71 at interval end) because the arrangement took two successive forms of taking: first, centuries of computational and conceptual barring (no operand-zero, no positional arithmetic, algebra delayed), then a credit settlement that moved recognition from the generators to the receivers. Suppression (0.52 at end) is real but changed register over the interval, which is why suppression_requirement is tracked: enforcement capacity was built up through curriculum control to the municipal numeral bans (peak 0.78 near the transmission era), decayed as absorption won, and migrated into softer historiographical gatekeeping. Theater ratio (0.38 at end) tracks the growing share of activity that performed rather than functioned: fraud-prevention justifications for bans whose main effect was barrier maintenance, and nineteenth-century universal-reason narratives that performed the credit settlement. Accessibility collapse is moderate (0.55): workable-if-inferior alternatives (board reckoning, Roman-numeral bookkeeping) persisted alongside, so alternatives narrowed but never fully closed. Resistance (0.55) reflects the algorist-versus-abacist rivalry and market-by-market clandestine adoption that ultimately broke the barrier. Time grid: one shared grid at t = 0, 10, 20, 30, 40, 50, 60 for all three tracked metrics; t0 approximates the fourth-century-BCE codification of the framework, t20 the eighth-to-ninth-century CE maturation of Indian and Arabic positional arithmetic, t30 the Liber Abaci-to-municipal-ban era (1202-1348), t40 the sixteenth-century absorption, t50 the nineteenth-century Eurocentric historiographical apex, t60 the late twentieth- and twenty-first-century comparative era. Endpoint values equal the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrator seats compute differently from the same structural data. From the scholastic administrator's position the framework was simply what number is — enforcing it was scholarship, not taking. From the computist's position it was a daily tax on every ledger. From the European tradition's retrospective position the episode is an inheritance it prefers to narrate as achievement; from the generator traditions' position it is unpaid recognition. The two institutional seats diverge despite equal nominal power because their exits differ: the administrators could reinterpret but not abandon the framework without dissolving their authority, while the receiving tradition cannot renounce the credit settlement without dissolving its self-image as an autonomous rational enterprise. The engine computes this per-seat divergence from the declared roles, powers, and exits; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Non-Western knowledge systems are declared beneficiaries because under this reading's corrected ledger the recognition flow runs to them — they are the owed creditors — while their stakeholder entry records the erasure they bore as a secondary payer position; the net-position ambiguity is carried as an omega rather than resolved by fiat. Scholastic authorities are beneficiaries of the barrier-era rents (epistemic monopoly over the ontology of quantity). European mathematical tradition is declared a victim — the dependency admission is a real cost this reading imposes on the tradition's self-account — while holding a secondary beneficiary position for the transmitted capability and the later credit stream; its effective directionality therefore sits short of the full-target end. Medieval computists and merchants are unambiguous targets: they bore the computational cost with no compensating flow. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by each seat's derived directionality and the arrangement's continental-to-global reach, which amplifies effective extraction by making verification of the credit question harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's founding mandate — securing deductive rigor by excluding the paradoxes that zero-as-quantity invited — is dead: modern foundations secure rigor by entirely different means, and nothing in contemporary mathematics is protected by the unit-plurality settlement. What persists is the residue: the credit settlement and the self-narrative it protects. The classification prevents two opposite mislabelings. Calling the whole complex pure extraction erases the genuine coordination achievement — the shared ontology of number and magnitude that made Greek deductive mathematics and the medieval curriculum possible at all. Calling it pure coordination erases the barring, the bans, and the erasure. The tangled-rope reading holds both, and the R5 interview locates the zombie component precisely: founding problem dead, world still arranged around the residue, which is the signature of a mandate outliving its function while its accounting machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_zero_as_number_entry,
    'This story instantiates one reading (contingent_thinkability_reading) of the kernel zero_as_number_entry; what would the sibling readings change structurally?',
    'Comparative-historiographical adjudication of the transmission record and of the framework-relativity of concept formation; convergence of Sanskrit-, Arabic-, and Latin-source scholarship on whether the transmitted item was a concept, a trigger, or an instance of a universally available structure.',
    'Under the universal-discovery sibling the victim set loses its dependency-admission element and the priority-recognition asymmetry collapses into mere chronology, dropping epsilon sharply; under the hybrid-scaffolding sibling transmission becomes trigger-not-transfer, shrinking the extraction claim to the scaffolding gap and repositioning the European seat from receiver to recognizer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position_zero_as_number_entry, conceptual, 'Committer structure: this is one reading of a contested kernel; sibling readings alter the victim/beneficiary structure and the epsilon of the episode.').

omega_variable(
    diophantine_partial_breach,
    'Does Diophantus''s arithmetical practice and late Alexandrian computational notation constitute a partial indigenous breach of the Greek barrier, weakening the strong impossibility claim?',
    'Philological determination of whether the Diophantine null-sign functioned as an operand-grade number inside calculations or only as a placeholder marking absence.',
    'If operand-grade, the reading softens from impossibility to severe-impedance; epsilon drops moderately and the dependency-admission victimhood narrows toward a delay claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diophantine_partial_breach, empirical, 'Whether the Greek lineage produced any indigenous proto-zero of operational grade.').

omega_variable(
    counterfactual_indigenous_emergence_untestable,
    'The claim that absent transmission zero-as-number would not have emerged in Europe rests on a counterfactual that cannot be directly tested; how strong is the inference from framework analysis to impossibility?',
    'Comparative natural experiments: Byzantium held the Greek framework plus contact with both sides and received rather than generated; internal Latin attempts to derive positional methods stalled until translations arrived. Supplement with framework-logical analysis of which Aristotelian commitments exclude an operand-zero.',
    'If the inference weakens from impossibility to high-probability blockage, the reading converges toward the scaffolding account and the European victim position softens from unable-to-generate to slow-to-generate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_indigenous_emergence_untestable, empirical, 'Strength of the necessity claim in the absence of a runnable counterfactual.').

omega_variable(
    erasure_layer_decomposition,
    'Is the historiographical credit misallocation part of this constraint''s extraction, or a separate downstream constraint in the same family?',
    'Epsilon-invariance test: author the erasure arrangement as its own story with its own stakeholders (textbook publishers, examination boards, national academies); if its epsilon and victim structure differ systematically from the barrier-era arrangement, split permanently and link via network edges.',
    'If split, this story''s epsilon falls toward the barrier-era level and the credit-stream capture moves to the sibling story''s receipt surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_layer_decomposition, conceptual, 'Whether barrier and erasure are one arrangement or a two-story family.').

omega_variable(
    nonwestern_net_position_ambiguity,
    'The non-Western knowledge systems are declared beneficiaries (priority recognition is owed to them and flows to them under this reading''s corrected ledger) while simultaneously bearing the erasure cost; is their net structural position beneficiary or payer?',
    'Specify the accounting period explicitly: during the barrier and erasure eras they were net payers; under the corrected accounting this reading institutes, they are net creditors. The declaration encodes the corrected ledger, not the historical cash flow.',
    'If read as net payers, their directionality approaches the target end and the arrangement reads as more purely extractive; if read as net beneficiaries, the extraction concentrates on the European seats and the recognition flow dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonwestern_net_position_ambiguity, preference, 'Net position of the generator traditions under competing accounting conventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__contingent_thinkability_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t10, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(zero_tr_t10, observed).
narrative_ontology:measurement(zero_tr_t20, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(zero_tr_t20, observed).
narrative_ontology:measurement(zero_tr_t30, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(zero_tr_t30, observed).
narrative_ontology:measurement(zero_tr_t40, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(zero_tr_t40, observed).
narrative_ontology:measurement(zero_tr_t50, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement_basis(zero_tr_t50, observed).
narrative_ontology:measurement(zero_tr_t60, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(zero_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t10, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(zero_be_t10, observed).
narrative_ontology:measurement(zero_be_t20, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(zero_be_t20, observed).
narrative_ontology:measurement(zero_be_t30, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement_basis(zero_be_t30, observed).
narrative_ontology:measurement(zero_be_t40, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(zero_be_t40, observed).
narrative_ontology:measurement(zero_be_t50, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement_basis(zero_be_t50, observed).
narrative_ontology:measurement(zero_be_t60, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(zero_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t10, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(zero_su_t10, observed).
narrative_ontology:measurement(zero_su_t20, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(zero_su_t20, observed).
narrative_ontology:measurement(zero_su_t30, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(zero_su_t30, observed).
narrative_ontology:measurement(zero_su_t40, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(zero_su_t40, observed).
narrative_ontology:measurement(zero_su_t50, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(zero_su_t50, observed).
narrative_ontology:measurement(zero_su_t60, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(zero_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__contingent_thinkability_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry__universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how zero entered Europe' covers three structurally distinct claims with distinct epsilon values and distinct victim/beneficiary structures: (1) this story — framework-gated thinkability, transmission necessary, indigenous emergence impossible, with a recognition debt attached to reception; (2) universal_discovery_reading — availability is a logical consequence of positional notation plus arithmetic operations, holder priority is mere chronology, no dependency admission and no recognition debt; (3) hybrid_scaffolding_reading — the concept was latent in positional notation, transmission triggered recognition rather than transferring a concept, and the extraction claim shrinks to a scaffolding gap. Measuring the episode by ontological availability yields near-zero extraction; measuring it by framework-gated generation and credit settlement yields high extraction. By the epsilon-invariance principle these are different constraints sharing one colloquial label, authored as separate files and linked here. The upstream story in the family is the universal-availability claim (highest empirical confidence on the mathematics itself); this reading sits downstream of it, contesting its historiographical use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
