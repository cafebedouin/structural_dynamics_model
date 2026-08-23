% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Clause 39 Universal Due Process Constraint (Universal-Rights Reading)
 *   domain: constitutional law / legal history / political theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the magna_carta_1215 kernel: the
 *   universal_rights_reading, under which Clause 39 — 'No free man shall be
 *   seized or imprisoned, or stripped of his rights or possessions, or
 *   outlawed or exiled... except by the lawful judgment of his equals or by
 *   the law of the land' — emits a transhistorical due process constraint
 *   binding all state power over all persons. Under this reading 'free men'
 *   denotes all persons, so the beneficiary set is the general populace and
 *   the payer set is the sovereign power the arrangement binds; coverage
 *   extends to arbitrary detention, extrajudicial punishment, and
 *   dispossession for everyone within the state's reach. The sibling readings
 *   are separate constraint stories in the same family, linked by network
 *   edges: the baronial_privilege_reading instantiates a different constraint
 *   with a narrow contracting-party beneficiary set and the crown as sole
 *   payer (its epsilon is authored for that different referent), and the
 *   living_document_reading instantiates a third whose epsilon tracks the
 *   interpretive tradition's current output. This file's epsilon (0.38) is
 *   authored only for the universal arrangement as this reading assesses it;
 *   the committer deltas are routed to omega variables, not folded into this
 *   classification. Claim and metrics are authored independently: the
 *   constraint is CLAIMED as tangled_rope — a genuine universal coordination
 *   function with real extraction riding on it — while the metrics describe
 *   its actual operation, including the modern plea machinery's extraction
 *   and the rising theater of pro forma process, without being tuned toward
 *   any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - judiciary: agenda-setter (institutional/identity_locked) — administers and defines the arrangement, collects institutional power from its operation
 *   - general_populace: primary beneficiary (moderate/constrained) — holds the protection, staffs the juries, funds the courts
 *   - sovereign_executive: primary target (institutional/trapped) — surrenders arbitrary detention and punishment power
 *   - security_apparatus: acute target (institutional/constrained) — bears operational curtailment, builds process workarounds at global scope
 *   - legal_profession: secondary beneficiary and receipt seat (organized/mobile) — collects fees and employment from the machinery
 *   - detainees_and_defendants: protected seat (powerless/trapped) — the arrangement's intended shield, also bearing machinery costs
 *   - noncitizen_detainees: excluded seat (powerless/trapped) — outside the effective coverage despite the universal premise
 *   - constitutional_historians: analytical observer (analytical/analytical) — sees the full three-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Clause 39 Universal Due Process Constraint (Universal-Rights Reading)").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional law / legal history / political theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '0d617534-9b7e-4e39-8c47-fa860ea109d8').
narrative_ontology:cs_kernel_codification('0d617534-9b7e-4e39-8c47-fa860ea109d8', fixed_text).
narrative_ontology:cs_authority_grounding('0d617534-9b7e-4e39-8c47-fa860ea109d8', lineage).
narrative_ontology:cs_interpretation_layer_present('0d617534-9b7e-4e39-8c47-fa860ea109d8').
narrative_ontology:cs_reading_relation('0d617534-9b7e-4e39-8c47-fa860ea109d8', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('0d617534-9b7e-4e39-8c47-fa860ea109d8', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('0d617534-9b7e-4e39-8c47-fa860ea109d8', foundational, free_men_denotes_all_persons).
narrative_ontology:cs_axiom_status(free_men_denotes_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('0d617534-9b7e-4e39-8c47-fa860ea109d8', free_men_denotes_all_persons, deontological).
narrative_ontology:cs_axiom('0d617534-9b7e-4e39-8c47-fa860ea109d8', foundational, clause39_principle_transcends_original_parties).
narrative_ontology:cs_axiom_status(clause39_principle_transcends_original_parties, holdable).
narrative_ontology:cs_axiom_grounding('0d617534-9b7e-4e39-8c47-fa860ea109d8', clause39_principle_transcends_original_parties, deontological).
narrative_ontology:cs_reference_frame('0d617534-9b7e-4e39-8c47-fa860ea109d8', transhistorical_universal_rights_grant).
narrative_ontology:cs_drift_state('0d617534-9b7e-4e39-8c47-fa860ea109d8', contemporary_mass_procedure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d617534-9b7e-4e39-8c47-fa860ea109d8', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, general_populace).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, detainees_and_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, sovereign_executive).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, security_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, sovereign_executive).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, detainees_and_defendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets what 'lawful judgment of his equals' and 'the law of the land' require, issues writs of habeas corpus, reviews executive detention, and defines the procedure the charter's rule demands. Its authority, docket, and institutional standing expand with the rule's reach, and it absorbs political retaliation when it enforces against the executive. Leaving the role would mean dissolving the institution whose identity is guardianship of lawful process.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judiciary, beneficiary).

% Holds the guarantee: liberty and property cannot be taken without lawful judgment of equals or the law of the land. Serves as the 'equals' through jury service, funds the courts through taxation, and bears the machinery's costs indirectly. Individually each person is exposed to state power; collectively the protection holds through the process the rule guarantees and the suffrage and jury coalition behind it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, general_populace, beneficiary,
    moderate, biographical, constrained, national).

% Bears the rule's core cost: it may not seize, imprison, dispossess, or punish anyone except through lawful judgment or the law of the land. It gains legitimacy and order from operating under the requirement, but its arbitrary coercive capacity is what the rule takes from it, and leaving the arrangement means constitutional rupture or open lawlessness rather than ordinary exit.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, sovereign_executive, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, sovereign_executive, beneficiary).

% The seat that feels the requirement most acutely: it seeks to detain and punish on operational timelines, and every demand for judgment, counsel, and review slows or blocks that work. It builds workarounds — emergency designations, offshore facilities, military commissions — and litigates for authority to detain without process, operating well beyond its home jurisdiction where oversight is thinnest.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, security_apparatus, payer,
    institutional, immediate, constrained, global).

% Collects fees and employment from the machinery: every writ, hearing, motion, and defense is billable work, and its livelihood scales with procedural complexity. It staffs the machinery's operation and advocates for its expansion. Its skills transfer to any legal order, so leaving is easy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% The people the guarantee exists to protect: facing seizure, imprisonment, or punishment, they hold the right to lawful judgment of equals or the law of the land. In custody they cannot leave; the requirement is the difference between process and bare state power over them. They also carry the machinery's costs — counsel fees, years of delay, and the pressure to plead that resolves most cases without the judgment the text promises.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, detainees_and_defendants, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, detainees_and_defendants, payer).

% Held at offshore facilities or in administrative detention under designations — enemy combatant, security threat, unlawful presence — that place them outside the requirement's effective coverage. They would claim that 'all persons' includes them; they lack standing, presence, and political voice in the forums where the coverage boundary is set.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, noncitizen_detainees, excluded,
    powerless, immediate, trapped, global).

% Tracks the charter's transmission from baronial instrument to constitutional icon: which clauses carry live legal force, which are ceremony, and what 'free men' bound in 1215 versus binds now. Sees the whole three-reading contest from outside any of the seats.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, legal_profession).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of arbitrary state power: by requiring that any deprivation of liberty, property, or standing pass through lawful judgment of equals or the law of the land, it gives every person subject to state power a predictable, contestable process, gives the state a legitimacy-preserving procedure for coercion, and stabilizes mutual expectations between rulers and ruled across generations.
% TRANSFER_FUNCTION: Moves procedural compliance and its costs from the sovereign and from litigants into the machinery: the state surrenders arbitrary detention and punishment capacity and funds courts; defendants pay counsel fees, delay, and plea pressure; liberty-protection flows to all persons; fees and employment accrue to the legal profession and institutional power to the judiciary.
% ABSENT_VOICES: Noncitizen detainees and administratively processed defendants would object that the universal reading's 'all persons' premise is not being honored for them — they are held or processed outside the effective coverage and are absent from the political forums where the boundary is set; courts hear their cases individually but the machinery's routine operation proceeds without their voice. Historically, the classes the text's original scope excluded (unfree tenants, women, the colonized) were absent from Runnymede and from most of the arrangement's subsequent authorship; the universal reading's claim is precisely that they should have been inside it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, executive detention power would be limited only by prudence and politics: habeas corpus, judicial review of detention, and constitutional due process clauses are load-bearing structures, and their removal would reorganize the courts' role, the legal profession's economy, and every person's exposure to state punishment. The world would rearrange around bare sovereign power.
% FOUNDING_PROBLEM: King John's arbitrary seizures, imprisonments, exactions, and punishments — crown power over persons and property exercised without lawful judgment — which the 1215 settlement was built to subject to process: no seizure, imprisonment, dispossession, outlawry, exile, or destruction except by lawful judgment of equals or the law of the land.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: pre-1215 chronicles (Roger of Wendover, the Barnwell chronicler) attesting John's exactions and imprisonments; the security apparatus's own recurring requests for process-free detention authority, which presuppose the pull of arbitrary power; and the modern litigation record in which executives claim detention authority outside process. The founding problem's persistence is attested by the enforcement docket itself — a dead founding problem generates no habeas petitions.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects a protective core with substantial machinery extraction: the arrangement genuinely shields persons from arbitrary seizure, but its operation extracts procedural compliance and fees — from the sovereign (compliance burden), from litigants and defendants (counsel costs, delay, plea pressure) — and those flows accrue to the legal machinery. Suppression (0.55) is the raw structural coercive force that holds the state to process — habeas, contempt, judicial review — authored unscaled; the engine owns directionality and scope scaling of extractiveness, and suppression is not scaled by either. Theater_ratio (0.40) tracks the growing share of process that is ritual rather than protection: jury trial — the 'lawful judgment of his equals' — now resolves a small minority of dispositions in mass-processing jurisdictions, displaced by plea negotiation that delivers the form of process without the text's judgment. Accessibility_collapse (0.40) is moderate because alternatives to due process persist and are repeatedly re-derived (emergency designations, offshore facilities, administrative detention) rather than collapsed by the arrangement's logic. Resistance (0.50) is sustained: executives push back in every emergency era and the sibling readings contest the scope and content. All measurement series run on one shared time grid (1215, 1354, 1689, 1791, 1942, 2001, 2025) with every tracked metric authored at every point, so no metric's end-state value is silently substituted into earlier rows. The suppression_requirement series is cyclical rather than monotonic — enforcement force was born high (the 1215 settlement required armed baronial coercion of the crown), settled through the constitutional eras, and re-spikes in war and emergency periods (1942, 2001); the cycle is driven by external executive appetite for arbitrary power, not by the arrangement's own extraction mechanism, and the base_properties scalar reflects the current (post-2001 spike, partially settled) phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the sovereign_executive and security_apparatus seats the arrangement is experienced as extraction: an asset (arbitrary coercive capacity) surrendered under coercive enforcement, with exit requiring constitutional rupture. From the general_populace seat the same structure is protective coordination: process as the price of state power, cheap at the individual level and rarely invoked. From the judiciary seat the arrangement is the source of its own authority — administering it is not a cost but the role's content — though enforcement against a hostile executive exposes it to retaliation, so even the agenda-setter seat carries a target-side component. From the detainees_and_defendants seat the arrangement is existential and double-edged: the difference between process and bare power, and also — through plea pressure and delay — a machinery that can extract surrender of the very rights it promises. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: general_populace and detainees_and_defendants hold the protection (d near the beneficiary end; detainees fully trapped into it), legal_profession collects fees from the machinery (near-full beneficiary, mobile exit), and the judiciary collects institutional power (beneficiary by derivation, moderated by enforcement retaliation). Payers: sovereign_executive bears the core cost — surrendered arbitrary power — with no exit short of rupture (near-full target), and security_apparatus bears the acute operational cost (near-full target at its seat, operating at global scope where verification is hardest, which the engine's scope amplification registers). Noncitizen_detainees are excluded rather than coordinated: the universal premise nominally covers them but they sit outside the effective operation, and that gap is itself part of what this reading's contest is about. No directionality overrides are authored: the derivation from the declared beneficiary/victim structure plus exit options captures the seat differences, and the override mechanism is keyed to coarse power atoms that would smear across differently-positioned institutional seats — judiciary, sovereign, and security apparatus all hold 'institutional' power but sit at opposite ends of d. Receipt: the extraction's gains demonstrably accrue to the legal_profession (fees and employment scaling with procedural complexity); the judiciary's gain is institutional power rather than the extracted flows and is recorded in its stakeholder situation rather than as the receipt seat. Fixing is prohibitive for whoever could fix it: the sovereign could in principle legislate the requirement away, but the arrangement is load-bearing for the legal order's legitimacy and the populace's coalition (jury service, suffrage, courts) makes removal a rupture, not a repair.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary state power over persons and property without lawful judgment — is live, not dead: every emergency era re-litigates it, and the enforcement docket (habeas petitions, detention challenges) is its direct attestation. Because the founding problem is live and the disappearance verdict is world_rearranges, the R5 mismatch consumer finds no zombie flag and mandatrophy is not resolved. The classification prevents two opposite mislabels: reading the arrangement as a pure coordination mechanism would erase the machinery's real extraction (plea pressure, legal rents, the state's surrendered asset) and miss the signature that someone is coordinated while someone pays through the same structure; reading it as pure extraction would erase the genuine, broadly distributed coordination function that no rentier controls and that the populace would re-derive if lost. The rising theater_ratio is the series to watch for mandatrophy onset: if process becomes predominantly ritual (theater_ratio sustained above 0.5) while the founding problem stays live, the arrangement would be drifting toward performance of due process without its function, and the plea-machinery omega is the resolution path for that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the magna_carta_1215 kernel — the universal_rights_reading (''free men'' = all persons; Clause 39 binds all state power for all persons). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the constraint family: the baronial_privilege_reading (protection limited to contracting parties — narrow beneficiary set, crown as sole payer) and the living_document_reading (content fixed by interpretive tradition, not the text''s principle) each yield different beneficiary/victim sets and different epsilon; family-level comparison resolves which reading the operative legal force actually tracks.',
    'If the baronial reading captured operative force, the beneficiary set collapses to landholding elites and the arrangement becomes a closed rent structure; if the living_document reading captured it, epsilon tracks the tradition''s current output rather than the text''s principle. This story''s epsilon (0.38) is authored for the universal reading''s arrangement only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of the Magna Carta kernel; sibling readings would change the constraint''s scope and content.').

omega_variable(
    transhistorical_binding_force,
    'Does Clause 39 itself bind modern states as a transhistorical constraint, or is the operative binding force carried entirely by derivative enactments (habeas corpus statutes, constitutional due process clauses) with the charter functioning as ceremony?',
    'Trace citation practice: when courts strike detention as unlawful, do they rest on the charter''s principle or exclusively on derivative statutes? If every operative ruling has a derivative statutory ground and the charter appears only in rhetoric, the transhistorical claim is symbolic.',
    'If derivative-only, this story''s arrangement is a descendant of the charter rather than its emission, and its persistence depends on ordinary legislative maintenance (raising repeal exposure); if the text itself binds, the arrangement carries the entrenchment the universal reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transhistorical_binding_force, conceptual, 'Whether the 1215 text itself is operative or its force is entirely derivative of later enactments.').

omega_variable(
    free_men_universalization_ambiguity,
    'Is ''free men'' = all persons a faithful reading of the text''s principle, or an anachronistic projection the 1215 parties and the text''s original scope do not support?',
    'Philological and doctrinal analysis of the charter''s 1215 scope against its later interpretive uptake; test whether the universal reading can be grounded in the text''s principle rather than its parties'' intent.',
    'If anachronistic, the universal coverage claim rests on later enactments and collapses into the transhistorical_binding_force question; if faithful, the universal scope is text-grounded and the baronial scope premise is foreclosed as a matter of the text''s own logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_universalization_ambiguity, conceptual, 'The core interpretive ambiguity on which this reading''s expansive coverage claim rests.').

omega_variable(
    plea_machinery_extraction_share,
    'What share of the arrangement''s current operation protects liberty through lawful judgment, versus extracts pleas and fees through process pressure (mass plea bargaining, procedural delay as leverage)?',
    'Empirical study of disposition pathways: trial rates, plea-coercion indicators (innocence-plea rates, sentence differentials between trial and plea), and the distribution of defense costs across defendant classes.',
    'A high extraction share would push effective extraction at the defendant seat toward full-target and raise theater_ratio past the Goodhart threshold — process delivered as form without the text''s judgment of equals; a low share supports the coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plea_machinery_extraction_share, empirical, 'The protective-versus-extractive composition of the modern procedural machinery.').

omega_variable(
    equals_composition_ambiguity,
    'Who counts as one''s ''equals'' for lawful judgment — and does jury composition, historically (propertied, male, racially filtered) and currently, satisfy the ''judgment of his equals'' premise for all persons this reading covers?',
    'Comparative analysis of jury composition against defendant demographics across jurisdictions and eras, plus doctrinal analysis of what ''equals'' requires.',
    'If ''equals'' systematically fails for classes of defendants, the protection is unevenly real: the universal premise is honored in scope but not in operation, raising effective extraction at those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equals_composition_ambiguity, empirical, 'Whether the judgment-of-equals premise is operationally satisfied for the full population the universal reading claims to cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1354, magna_carta_1215__universal_rights_reading, theater_ratio, 1354, 0.14).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__universal_rights_reading, theater_ratio, 1689, 0.16).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_1215__universal_rights_reading, theater_ratio, 1791, 0.12).
narrative_ontology:measurement(magn_tr_t1942, magna_carta_1215__universal_rights_reading, theater_ratio, 1942, 0.22).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_1215__universal_rights_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.32).
narrative_ontology:measurement(magn_be_t1354, magna_carta_1215__universal_rights_reading, base_extractiveness, 1354, 0.3).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__universal_rights_reading, base_extractiveness, 1689, 0.24).
narrative_ontology:measurement(magn_be_t1791, magna_carta_1215__universal_rights_reading, base_extractiveness, 1791, 0.18).
narrative_ontology:measurement(magn_be_t1942, magna_carta_1215__universal_rights_reading, base_extractiveness, 1942, 0.28).
narrative_ontology:measurement(magn_be_t2001, magna_carta_1215__universal_rights_reading, base_extractiveness, 2001, 0.34).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1354, magna_carta_1215__universal_rights_reading, suppression_requirement, 1354, 0.55).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__universal_rights_reading, suppression_requirement, 1689, 0.45).
narrative_ontology:measurement(magn_su_t1791, magna_carta_1215__universal_rights_reading, suppression_requirement, 1791, 0.35).
narrative_ontology:measurement(magn_su_t1942, magna_carta_1215__universal_rights_reading, suppression_requirement, 1942, 0.45).
narrative_ontology:measurement(magn_su_t2001, magna_carta_1215__universal_rights_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__universal_rights_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition (epsilon-invariance): the colloquial label 'Magna Carta' covers three structurally distinct constraints. This file instantiates the universal_rights_reading only — epsilon (0.38) is authored for the universal due process arrangement as this reading assesses it, with the general populace as beneficiary set and the sovereign as payer set. The baronial_privilege_reading instantiates a different constraint: narrow contracting-party beneficiary set, crown as sole payer, contract-enforcement function — its epsilon and classification are its own. The living_document_reading instantiates a third: content fixed by interpretive tradition, epsilon tracking the tradition's current output. The upstream/downstream structure runs text to tradition to doctrine: this reading's universal scope claim forecloses the baronial scope claim in any single framework, while its doctrinal output supplies the living-document reading's precedential material. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
