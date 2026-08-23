% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Obligations Binding the Pharaoh
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This story instantiates the RECIPROCITY READING of the Ma'at kernel:
 *   Ma'at imposes mutual obligations, the Pharaoh must provide justice,
 *   stability, and proper resource distribution to maintain cosmic balance,
 *   and failed obligations justify resistance or withdrawal of support. The
 *   standing arrangement under contest — the referent for epsilon — is the
 *   historical Egyptian settlement in which surplus and labor flow upward
 *   under a normative framework that also binds the ruler. Per the
 *   epsilon-invariance principle, the colloquial label 'Ma'at' covers
 *   multiple structurally distinct claims, so the kernel decomposes into
 *   three linked stories: this reciprocity reading (moderate, ceiling-bound
 *   extraction with a real coordination function), a divine-mandate reading
 *   (ruler definitionally incapable of violation — no ceiling, extraction
 *   unchecked by obligation), and a distributed-maintenance reading
 *   (obligation diffused across all stations). Each carries its own epsilon,
 *   beneficiaries, and classification; they are linked via
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope from the authoring seat — genuine
 *   reciprocal coordination plus asymmetric upward extraction held by active
 *   enforcement — while the metrics describe the arrangement's actual
 *   operation independently.
 *
 * KEY AGENTS:
 *   - - pharaoh_and_royal_court: Agenda-setter and principal collector (institutional / identity_locked) — administers the settlement, collects its largest flows, and is itself bound by its obligations under this reading
 *   - - temple_priesthood: Primary beneficiary (powerful / constrained) — collects estates, offerings, and exemptions; co-authors the normative discourse
 *   - - provincial_elites_nomarchs: Dual-positioned beneficiary/payer (powerful / constrained) — collect locally, remit upward, and are the operational channel of withdrawal when obligations fail
 *   - - scribe_administrative_class: Beneficiary (organized / constrained) — staffs the assessment machinery; careers ride on continuation
 *   - - peasant_farming_households: Primary target (powerless / trapped) — bears the grain assessment; receives the reciprocal goods only when obligations are honored
 *   - - corvee_laborers: Target with no voice (powerless / trapped) — bears the physical cost of the monument program; excluded from term-setting
 *   - - egyptological_historiography: Analytical observer (analytical / analytical) — reads the full structure from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.58).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.62).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Obligations Binding the Pharaoh").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808').
narrative_ontology:cs_kernel_codification('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', distributed).
narrative_ontology:cs_authority_grounding('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', lineage).
narrative_ontology:cs_interpretation_layer_present('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808').
narrative_ontology:cs_reading_relation('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', foundational, pharaoh_bound_by_ma_at_obligations).
narrative_ontology:cs_axiom_status(pharaoh_bound_by_ma_at_obligations, holdable).
narrative_ontology:cs_axiom_grounding('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', pharaoh_bound_by_ma_at_obligations, deontological).
narrative_ontology:cs_axiom('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', foundational, failed_obligations_justify_withdrawal_of_support).
narrative_ontology:cs_axiom_status(failed_obligations_justify_withdrawal_of_support, holdable).
narrative_ontology:cs_axiom_grounding('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', failed_obligations_justify_withdrawal_of_support, instrumental).
narrative_ontology:cs_reference_frame('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', mutual_obligation_covenant).
narrative_ontology:cs_drift_state('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', first_intermediate_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1bd70e1-3e68-4c7d-9c3c-57a26ce2d808', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, provincial_elites_nomarchs).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, scribe_administrative_class).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasant_farming_households).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_elites_nomarchs).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, maat_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, conditional_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Personifies the office that administers the Ma'at settlement: sets the grain assessment, commands corvee levies, controls the state granaries, and appoints the judges and officials who deliver the justice the framework promises. Collects the largest share of the surplus. Under this reading the office is also bound: the king owes justice, stability, and distribution, and the tradition itself records kings failing that debt and paying for it in legitimacy. Abdication is not a live option and the office's identity is fused with its Ma'at duties, so there is no exit from the obligation structure — only performance or breach.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court, beneficiary).

% Holds endowed estates, receives offerings and tax exemptions, and staffs the cult that performs the daily rites said to sustain cosmic order. Gains land grants issued in exchange for ritual service and enjoys standing from its role as custodian of the Ma'at tradition. Substantial interpretive authority over what Ma'at requires sits here and in the scribal schools. Temple domains do not bear the grain assessment or corvee that lay households bear.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    powerful, generational, constrained, national).

% Administer the nomes: collect and forward grain tax, muster local labor quotas, and administer local justice, keeping a share of collections locally. Gain office, land, and hereditary standing from the arrangement while owing military and labor contingents upward. When the center weakens or overreaches, they retain collection locally and let remittances lapse — the documented channel through which withdrawal of support actually operated.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_elites_nomarchs, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_elites_nomarchs, payer).

% Staffs the census, assessment, and granary apparatus; literate numeracy is the scarce skill the entire transfer system runs on. Receives stipends, allotments, and exemption from corvee. Career prospects depend wholly on the continuation of the assessment-and-collection machinery, so the class has every incentive to keep the obligations flowing regardless of whether the ruler's side of the exchange is being honored.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, scribe_administrative_class, beneficiary,
    organized, biographical, constrained, national).

% Work the flood-fed fields, deliver the grain assessment, and supply household members for corvee seasons. Receive back, when obligations are honored, administered justice, maintained dykes and canals, and famine relief from state granaries. Flight from land and levy is possible in principle but means abandoning tenancy, kin networks, and subsistence, and there is no alternative sovereign to appeal to; the assessment arrives whether or not the ruler's side of the exchange has been kept.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasant_farming_households, payer,
    powerless, biographical, trapped, local).

% Drafted in rotating gangs for construction, quarrying, mining, and expeditionary labor. Bear the physical cost of the monument-and-infrastructure program directly and seasonally, away from their own fields. Have no seat anywhere the terms of obligation are set or interpreted; petitions run through the same officialdom that drafts them, and the record of their objections survives mainly as strike notices and litigation fragments.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, corvee_laborers, excluded).

% Reads the textual and archaeological record from outside the arrangement: instruction literature, royal inscriptions, strike papyri, granary archaeology, and settlement surveys of the decentralization episodes. Attests to the shape of the obligation structure and its breakdowns without holding any position inside it and without collecting or paying anything under it.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptological_historiography, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh_and_royal_court).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes reciprocal duty across the ruler-subject relationship and mobilizes surplus at civilizational scale: the grain assessment funds granaries that buffer Nile failure, corvee builds and maintains dykes, canals, granaries, and temples, and the shared Ma'at standard makes royal legitimacy legible — and contestable — to both sides of the exchange.
% TRANSFER_FUNCTION: Moves grain tax, corvee labor, and military service from farming households and drafted laborers upward to the palace, temple estates, and officialdom; moves justice administration, hydraulic maintenance, famine relief, and protection back down when the ruler's obligations are honored.
% ABSENT_VOICES: The households bearing tax and corvee have no seat where Ma'at's terms are authored or interpreted — normative discourse is produced by priests, scribes, and court instruction literature. Laborers and villagers speak only through petition channels controlled by the officialdom that assesses them, so their objections surface as strikes, flight, and litigation records rather than as voice in setting the obligations.
% DISAPPEARANCE_RATIONALE: Without the reciprocal-obligation framework, surplus mobilization loses its normative warrant: assessment becomes bare coercion, corvee loses its justification, and the granary-relief cycle that buffers Nile failure loses its fiscal base. Succession and jurisdiction revert to raw force; the documented pattern of such a loss is the First Intermediate Period's fragmentation into autonomous nome regimes.
% FOUNDING_PROBLEM: After unification, the Nile valley required a mechanism to legitimize large-scale surplus mobilization — for flood management, famine buffering, and defense — that no village or nome could provision alone, and to make obedience to a distant center tolerable rather than merely compelled.
% FOUNDING_PROBLEM_CORROBORATION: Instruction literature from the scribal seat (Instructions for Merikare; the Tale of the Eloquent Peasant's justice-before-the-official motif) treats the ruler's obligations as real and breachable rather than as palace rent-propaganda. Lament texts in the Ipuwer tradition and the material record of First Intermediate Period decentralization corroborate that contemporaries experienced the arrangement as conditional on performance. The cultivators themselves left no direct testimony — literacy excludes them — which is itself signal about who could author the surviving record.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but ceiling-bound (0.58): the assessment and corvee transfers are real and regressive, yet the reciprocity norm caps how far they can be pushed before withdrawal events reset them. Suppression (0.62) reflects the enforcement machinery — census, assessment squads, corvee muster, judicial apparatus — and the thinness of exit; note suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope, unlike extractiveness, which the engine scales by directionality and spatial scope. Theater ratio (0.38) splits the difference between functional delivery (granaries, dykes, courts) and performative maintenance (ritual, royal iconography, offering cults whose proportion grows as delivery strains). Accessibility collapse is moderate (0.45): alternatives — flight, heterodox practice, provincial autonomy — remain conceivable and historically exercised, which is precisely what distinguishes this construct from a natural law. Resistance (0.5) is documented: labor strikes, petition litigation, and nome-level remittance strikes. The measurement series run on ONE shared eleven-point grid so every metric is authored at every examined time point. The series are deliberately cyclical rather than monotonic: extraction accumulates toward a peak, a withdrawal event (provincial defection, strike wave, decentralization) knocks it back, a renegotiated settlement follows, and accumulation resumes. The oscillation is not noise — it is plausibly the ceiling mechanism itself operating, i.e., intermittent enforcement of the reciprocity clause; the phase of the last reset (t=27) is why the end-state scalar sits mid-range rather than at the t=24 peak.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the collector seats should compute differently, and the crown's seat should compute as internally split. From the peasant and corvee seats the arrangement operates as enforced transfer with conditional reciprocals they cannot enforce directly — a snare-flavored experience whenever the ruler's side lapses. From the priesthood and scribal seats it operates as a stable coordination order that funds their position — rope-flavored. The crown's seat is genuinely dual: it runs and collects the apparatus (agenda_setter/beneficiary) while carrying the obligation burden whose breach triggers justified withdrawal. Coalition power among the powerless victims is structurally limited — dispersed villages, seasonal labor gangs, no independent organization — which is why withdrawal historically ran through ELITE defection (nomarchs withholding remittance) rather than peasant combination; the engine should see the victims' d driven by trapped exit, not by collective leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-d seats: the priesthood (constrained exit, generational horizon) sits deep in beneficiary territory; the scribal class likewise. Victim declarations map to high-d seats: trapped peasants and drafted laborers with no alternative sovereign sit near the full-target end. The nomarchs derive as mildly beneficiary but their dual position (they remit upward and bear quota risk) pulls them toward symmetric — the structural data carries this via secondary_role. One override is declared: for the institutional power atom, d = 0.40. The automatic derivation would read the crown as a near-pure beneficiary — it heads the collection apparatus with effectively arbitrage-grade control — but under THIS reading the crown is itself a bound party: failed obligations justify withdrawal of support, the office cannot exit (identity_locked), and the tradition records kings paying legitimacy for breach. The derived d would therefore understate the crown's target-side exposure; the override corrects it to part-target. The priesthood and nomarchs are assigned the 'powerful' atom precisely so this override does not smear onto seats that bear no comparable obligation burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Without the beneficiary/coordination declarations, the arrangement would collapse into a pure-snare reading — upward transfer backed by coercion — erasing the real coordination function (famine buffering, hydraulic maintenance, legible justice) that made the settlement durable for centuries. Without the victim declarations and enforcement requirement, it would read as pure rope, erasing the regressive asymmetry that made withdrawal episodes recurrent. The founding problem remains LIVE (the Nile did not stop flooding and the coordination need did not lapse), so no mandatrophy declaration is authored; the R5 mismatch consumer should find status=live paired with verdict=world_rearranges — a consistent pairing, no zombie flag. The cyclical measurements guard against the opposite error: a monotonic-rising series would invite a tangled_rope-to-snare transition dating, but the resets are real structural events, not metric substitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the maat_order_principle kernel captures the OPERATIVE constraint — this reciprocity reading (ruler bound, breach actionable), the divine-mandate reading (ruler definitionally incapable of violation), or the distributed-maintenance reading (obligation diffused across all stations)?',
    'Textual-historical weighting of which normative claims had operative force: instruction literature and breach-and-restoration records versus unconditional royal ideology versus station-duty texts; cross-check against which reading the enforcement episodes actually presuppose.',
    'Under the divine-mandate reading the extraction ceiling vanishes and the crown''s d falls toward pure beneficiary, pushing the arrangement snare-ward; under distributed maintenance the ceiling diffuses and per-seat directionalities flatten. This story''s classification is valid ONLY within the reciprocity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this constraint is one reading of the Ma''at kernel; sibling readings would restructure beneficiary/victim topology and remove or relocate the extraction ceiling.').

omega_variable(
    reciprocity_ceiling_reality,
    'Did the obligation ceiling actually constrain extraction, or did it merely legitimize whatever extraction occurred after the fact?',
    'Compare reconstructed extraction levels immediately before and after documented withdrawal episodes (First Intermediate Period decentralization, labor strikes, remittance lapses): a consistent post-event reset indicates the ceiling had teeth; continued extraction through crises indicates ideological cover only.',
    'If cover-only, the constraint computes closer to snare (the coordination story as fig leaf); if the ceiling bit, tangled_rope stands with a functioning self-limiting mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_ceiling_reality, empirical, 'Whether the reciprocity norm was a binding constraint on extraction or post-hoc legitimation.').

omega_variable(
    cosmic_law_vs_constructed_norm,
    'Is the Ma''at framework, as participants held it, a cosmic-law-like structure (encountered as fact, like the flood cycle) or a constructed political theology serving identifiable beneficiaries?',
    'Separate the framework''s content into components tracking independently verifiable regularities (Nile behavior, agricultural cycles, storage economics) from components that are purely political (obedience duties, tribute sanctity); constructed components with identifiable beneficiaries indicate false-summit structure rather than natural law.',
    'If predominantly constructed with identifiable beneficiaries, any natural-law presentation of Ma''at is false-summit material and the constraint belongs firmly in the human-choice family; if substantially law-like in participants'' epistemic position, part of its persistence needs no enforcement explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_law_vs_constructed_norm, conceptual, 'Naturalness ambiguity: cosmic law versus constructed political theology with beneficiaries.').

omega_variable(
    withdrawal_enforcement_channel,
    'Through what channel does justified withdrawal of support actually operate — passive (tax evasion, flight), active (strikes, revolt), or structural (elite remittance strikes and provincial defection) — and is the threat credible enough to bind the ruler without organization among the powerless?',
    'Catalog documented withdrawal episodes and attribute each to a channel; test whether extraction concessions followed channels requiring peasant coordination (rare in the record) or elite defection (common).',
    'If withdrawal runs almost entirely through elite defection, the ceiling protects the ruler only as long as elites are retained — the constraint''s stability depends on managing a narrow beneficiary class, sharpening the tangled_rope reading; if passive channels alone sufficed, the ceiling approaches self-enforcement and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_enforcement_channel, empirical, 'Operational channel and credibility of the justified-withdrawal mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_reciprocity_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t0, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t3, maat_order_principle__reciprocity_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t3, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t6, maat_order_principle__reciprocity_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t6, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t9, maat_order_principle__reciprocity_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t9, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t12, maat_order_principle__reciprocity_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t12, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t15, maat_order_principle__reciprocity_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t15, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t18, maat_order_principle__reciprocity_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t18, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t21, maat_order_principle__reciprocity_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t21, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t24, maat_order_principle__reciprocity_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t24, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t27, maat_order_principle__reciprocity_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t27, observed).
narrative_ontology:measurement(maat_reciprocity_tr_t30, maat_order_principle__reciprocity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(maat_reciprocity_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(maat_reciprocity_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(maat_reciprocity_be_t0, observed).
narrative_ontology:measurement(maat_reciprocity_be_t3, maat_order_principle__reciprocity_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement_basis(maat_reciprocity_be_t3, observed).
narrative_ontology:measurement(maat_reciprocity_be_t6, maat_order_principle__reciprocity_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(maat_reciprocity_be_t6, observed).
narrative_ontology:measurement(maat_reciprocity_be_t9, maat_order_principle__reciprocity_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement_basis(maat_reciprocity_be_t9, observed).
narrative_ontology:measurement(maat_reciprocity_be_t12, maat_order_principle__reciprocity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(maat_reciprocity_be_t12, observed).
narrative_ontology:measurement(maat_reciprocity_be_t15, maat_order_principle__reciprocity_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(maat_reciprocity_be_t15, observed).
narrative_ontology:measurement(maat_reciprocity_be_t18, maat_order_principle__reciprocity_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement_basis(maat_reciprocity_be_t18, observed).
narrative_ontology:measurement(maat_reciprocity_be_t21, maat_order_principle__reciprocity_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement_basis(maat_reciprocity_be_t21, observed).
narrative_ontology:measurement(maat_reciprocity_be_t24, maat_order_principle__reciprocity_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(maat_reciprocity_be_t24, observed).
narrative_ontology:measurement(maat_reciprocity_be_t27, maat_order_principle__reciprocity_reading, base_extractiveness, 27, 0.55).
narrative_ontology:measurement_basis(maat_reciprocity_be_t27, observed).
narrative_ontology:measurement(maat_reciprocity_be_t30, maat_order_principle__reciprocity_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(maat_reciprocity_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_reciprocity_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(maat_reciprocity_su_t0, observed).
narrative_ontology:measurement(maat_reciprocity_su_t3, maat_order_principle__reciprocity_reading, suppression_requirement, 3, 0.54).
narrative_ontology:measurement_basis(maat_reciprocity_su_t3, observed).
narrative_ontology:measurement(maat_reciprocity_su_t6, maat_order_principle__reciprocity_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement_basis(maat_reciprocity_su_t6, observed).
narrative_ontology:measurement(maat_reciprocity_su_t9, maat_order_principle__reciprocity_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement_basis(maat_reciprocity_su_t9, observed).
narrative_ontology:measurement(maat_reciprocity_su_t12, maat_order_principle__reciprocity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(maat_reciprocity_su_t12, observed).
narrative_ontology:measurement(maat_reciprocity_su_t15, maat_order_principle__reciprocity_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(maat_reciprocity_su_t15, observed).
narrative_ontology:measurement(maat_reciprocity_su_t18, maat_order_principle__reciprocity_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement_basis(maat_reciprocity_su_t18, observed).
narrative_ontology:measurement(maat_reciprocity_su_t21, maat_order_principle__reciprocity_reading, suppression_requirement, 21, 0.61).
narrative_ontology:measurement_basis(maat_reciprocity_su_t21, observed).
narrative_ontology:measurement(maat_reciprocity_su_t24, maat_order_principle__reciprocity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(maat_reciprocity_su_t24, observed).
narrative_ontology:measurement(maat_reciprocity_su_t27, maat_order_principle__reciprocity_reading, suppression_requirement, 27, 0.6).
narrative_ontology:measurement_basis(maat_reciprocity_su_t27, observed).
narrative_ontology:measurement(maat_reciprocity_su_t30, maat_order_principle__reciprocity_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(maat_reciprocity_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Ma'at' decomposes into three structurally distinct claims per the epsilon-invariance principle. This reciprocity reading carries a moderate, ceiling-bound epsilon (0.58) because it locates an enforceable obligation ON the ruler. The divine-mandate sibling removes the ceiling entirely (ruler definitionally incapable of violation), yielding a higher epsilon over the same transfer flows; the distributed-maintenance sibling diffuses obligation across all stations, flattening per-seat directionalities and diluting specifically royal accountability. The upstream sibling (divine mandate) supplies the royal ideology that this reading's instruction-literature tradition pushes against; the two are cited as evidence for rival legitimacy doctrines and are therefore linked. Each story stands alone with its own beneficiaries, victims, and claimed type; no story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
