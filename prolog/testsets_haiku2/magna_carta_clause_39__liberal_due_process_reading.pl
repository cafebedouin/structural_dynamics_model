% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 Universal Due Process Rights (Liberal Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) stands at the foundation of English
 *   constitutional law. The Latin phrase 'nullus liber homo' ('no free man')
 *   shall be taken or imprisoned or dissiezed except 'per legem terrae' ('by
 *   the law of the land') has been read across eight centuries in radically
 *   different ways. This story instantiates the LIBERAL DUE PROCESS READING:
 *   the clause establishes a universal right of all subjects against
 *   arbitrary executive power, grounded in written law and enforced through
 *   courts. This reading expands Clause 39 from a feudal grant protecting
 *   baronial privilege into a universal shield protecting every subject. The
 *   reading vindicates a natural-rights and rule-of-law philosophy that sees
 *   law itself as the supreme authority, superior to executive will. Under
 *   this reading, the constraint is MASSIVELY EXTRACTIVE from executive
 *   discretion — it strips the Crown of the option to act arbitrarily — and
 *   it benefits all individual subjects by giving them a counterclaim
 *   grounded in law. The constraint's force has grown over centuries as
 *   common-law reasoning expanded it, statute has reinforced it, and
 *   international human-rights doctrine has universalized it further.
 *
 * KEY AGENTS:
 *   - Individual subjects (powerless, protected by the constraint)
 *   - Executive power / Crown (institutional, constrained by legal procedure requirement)
 *   - Feudal lords and local authorities (powerful, dispossessed of arbitrary discretion)
 *   - Parliament and judicial authority (institutional, elevated to interpretive authority)
 *   - Common-law jurists (organized, profiting from expanded legal interpretation)
 *   - Traditionalist hierarchists (powerful, contesting the reading)
 *   - Constitutional interpretive seat (analytical observer)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.82).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.71).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 Universal Due Process Rights (Liberal Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'f84963e1-f407-47b4-8128-9964800292df').
narrative_ontology:cs_kernel_codification('f84963e1-f407-47b4-8128-9964800292df', fixed_text).
narrative_ontology:cs_authority_grounding('f84963e1-f407-47b4-8128-9964800292df', lineage).
narrative_ontology:cs_interpretation_layer_present('f84963e1-f407-47b4-8128-9964800292df').
narrative_ontology:cs_reading_relation('f84963e1-f407-47b4-8128-9964800292df', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('f84963e1-f407-47b4-8128-9964800292df', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('f84963e1-f407-47b4-8128-9964800292df', foundational, legal_procedure_binds_all_authority).
narrative_ontology:cs_axiom_status(legal_procedure_binds_all_authority, holdable).
narrative_ontology:cs_axiom_grounding('f84963e1-f407-47b4-8128-9964800292df', legal_procedure_binds_all_authority, deontological).
narrative_ontology:cs_axiom('f84963e1-f407-47b4-8128-9964800292df', foundational, individual_subjects_possess_standing_against_state).
narrative_ontology:cs_axiom_status(individual_subjects_possess_standing_against_state, holdable).
narrative_ontology:cs_axiom_grounding('f84963e1-f407-47b4-8128-9964800292df', individual_subjects_possess_standing_against_state, deontological).
narrative_ontology:cs_axiom('f84963e1-f407-47b4-8128-9964800292df', foundational, written_law_supersedes_discretionary_will).
narrative_ontology:cs_axiom_status(written_law_supersedes_discretionary_will, holdable).
narrative_ontology:cs_axiom_grounding('f84963e1-f407-47b4-8128-9964800292df', written_law_supersedes_discretionary_will, conventional).
narrative_ontology:cs_reference_frame('f84963e1-f407-47b4-8128-9964800292df', universal_legal_constraint_on_executive).
narrative_ontology:cs_drift_state('f84963e1-f407-47b4-8128-9964800292df', contemporary_constitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f84963e1-f407-47b4-8128-9964800292df', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, individual_subjects_protected).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_power_constrained).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, feudal_arbitrators_dispossessed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, common_law_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every subject within the realm gains a claim that their property and liberty cannot be taken without 'the law of the land' — a universal right that applies equally to peasant and merchant. The constraint transforms from a feudal grant of hierarchical privilege into a standing protection against arbitrary executive action. Under this reading, subjects exit arbitrary power not by leaving the realm but by invoking the law itself as a shield, an option that did not formally exist before. Over 800 years, this seat's protection expands from a narrow feudal right to a universal due-process principle covering criminal procedure, personal liberty, and statutory interpretation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, individual_subjects_protected, beneficiary,
    powerless, civilizational, constrained, national).

% The Crown and its agents are bound by legal procedure in matters of property seizure and personal detention. The executive loses the option to act on discretionary will alone; it must demonstrate legal warrant. This constraint imposes compliance costs (judicial review, evidentiary process, petition rights) and eliminates revenue streams from arbitrary confiscation and summary imprisonment. The constraint's enforcement requires independent courts and a legal profession empowered to challenge executive action. The Crown retains the mobility to argue that 'necessity' or 'state of exception' justifies override — and in every generation it tries. But the standard for override has risen: it must now claim legal warrant even for exceptions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_power_constrained, payer,
    institutional, civilizational, mobile, national).

% Local lords, ecclesiastical authorities, and royal officials who once wielded unchecked power to seize, fine, or imprison subjects within their jurisdiction are stripped of that discretion. Their ability to extract feudal dues and enforce obedience through threat of dispossession is formally subordinated to a written law accessible to ordinary subjects. They retain power but lose the authority to define what constitutes legal cause; that authority migrates to courts and to the written legal text itself. By t800, this seat has been substantially stripped of feudal prerogatives; what remains is mediated through legal form.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_arbitrators_dispossessed, payer,
    powerful, generational, constrained, national).

% Parliament (and, over centuries, common-law courts) becomes the body that authoritatively interprets what 'the law of the land' means and what constitutes due process. The constraint creates a new institutional power: the power to speak for the written law and to bind executive action through legal interpretation. Parliament gains authority precisely by constraining the Crown. Over 800 years Parliament evolves from a body convened at the Crown's pleasure to a co-equal branch of government. Clause 39 is the textual foundation that enables this transition.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, parliamentary_interpretive_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Judges, lawyers, and legal scholars become essential interpreters of the constraint. They build careers and professional authority around explaining what 'law of the land' means in concrete cases. The constraint creates a new profession (or expands an existing one) whose power and status rest on their role as custodians of legality. They are both beneficiaries of the constraint's existence and enforcers of it through litigation and judgment. The common-law tradition itself is the jurists' creation: they took Clause 39 and wove it into a continuous body of precedent that expanded its reach across centuries. Without the jurists' interpretive labor, the constraint would be dead text.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, common_law_jurists, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, common_law_jurists, agenda_setter).

% Those already stripped of property or imprisoned before the constraint took effect are not retroactively restored. The constraint protects against FUTURE arbitrary action but does not remedy past harm. They would object if they had voice — the constraint's protection is prospective only, which leaves accumulated injustice unaddressed. Their exclusion from remedy is a structural feature of how the constraint operates. This seat is DEFINITIONALLY absent from any negotiation of the constraint; they cannot invoke Clause 39 for wrongs already done.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, dispossessed_subjects_under_prior_regime, excluded,
    powerless, biographical, trapped, national).

% Traditionalist lords and clergy who see the constraint as a violation of the natural hierarchical order, who argue that subjects owe obedience to their assigned superiors, and that invoking 'law' to challenge executive will is seditious. They contest whether the constraint is a legitimate development of feudal customary rights or a revolutionary rupture. Their objection is ideological — they hold that hierarchy and arbitrary superior judgment are the foundation of order, not threats to it. Over 800 years this seat has lost institutional power but remains a permanent minority objection.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_hierarchy_traditionalists, observer,
    powerful, generational, constrained, national).

% From this seat we trace how the constraint has been read and re-read across centuries: expanded to cover criminal process, property rights, and personal liberty; contracted during emergencies and wars; contested in each generation. The constraint's shape depends on who interprets it. This seat is the reflexive position from which all three readings of Clause 39 are visible simultaneously.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_interpreter_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, parliamentary_interpretive_authority).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, legally binding procedure for any executive action that affects property or liberty. Instead of dispersed, arbitrary power held by lords and royal officials, the constraint centralizes the standard: no seizure without legal warrant, accessible to subjects through courts and petition. This solves the coordination problem of 'what counts as legitimate authority' — the answer is 'lawful procedure,' not 'superior will.'
% TRANSFER_FUNCTION: Transfers authority from scattered feudal actors (lords, officials) to a centralized legal system and Parliament. It also transfers PROTECTION to individual subjects: they gain a counterclaim against arbitrary power. The constraint moves the right to define legality from executive discretion to written law and its interpreters.
% ABSENT_VOICES: Dispossessed subjects whose property was seized before the constraint took effect; feudal authorities who see the constraint as revolutionary sedition against natural hierarchy; those who would be imprisoned without trial under an older regime and now invoke the constraint are materially present (not absent), but traditionalist voices contesting whether legal procedure is a legitimate limit on hierarchical authority remain marginal or suppressed. They could argue that obedience to superiors is the foundational value, not legal procedure.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the Crown could seize property and imprison subjects at will, with no legal requirement to show cause — the entire structure of English constitutional development would be reversed. Centuries of common-law reasoning, parliamentary authority, and subject protections would evaporate. The Crown would revert to arbitrary rule; subjects would have no standing to invoke law against it. The institutional landscape of courts, Parliament, and the legal profession itself rests on this constraint's persistence.
% FOUNDING_PROBLEM: In the decades before 1215, the Crown seized properties from nobles, imprisoned subjects without cause, and levied arbitrary fines. The feudal contract was supposed to require the Crown to respect established customs and seek counsel; in practice, the Crown ignored both. Nobles faced dispossession; subjects faced arbitrary imprisonment; ecclesiastical lands faced confiscation. There was no written recourse, no court a subject could invoke against the Crown's will.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers contemporary to 1215 (Roger of Wendover, Matthew Paris) document widespread grievances about arbitrary seizure and imprisonment. Later jurists like Coke and Blackstone affirm that the founding problem — unchecked executive discretion — persists in each era and that Clause 39 is the answer. Modern constitutional lawyers and historians outside the benefiting institutions (independent scholars, civil liberties organizations, international human-rights bodies) continue to invoke it as a standing protection against executive overreach. The founding problem is live because executive power has never ceased trying to exempt itself from legal constraint.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading is claimed as Tangled Rope because it solves a GENUINE coordination problem (establishing legality as the binding standard for authority) WHILE EXTRACTING from executive discretion and feudal arbitrariness. The beneficiaries (individual subjects, Parliament, common-law jurists) are coordinated by the constraint — they gain a unified legal standard. The victims (executive discretion, feudal particularism) are dispossessed. The metrics reflect this dual structure: Extractiveness is HIGH (0.82 at the interval end) because the constraint progressively strips discretionary power from the Crown and feudal authorities over 800 years, centralizing authority in written law and courts. The Crown learns that it MUST justify action in legal terms and cannot simply act on will — this is extraction of a crucial option. Suppression starts low (0.45 at t0) because in 1215 the constraint is fragmentary and feudal lords can suppress or ignore it. By t800 suppression is high (0.71) because enforcing the constraint requires a standing legal profession, an independent judiciary, and Parliamentary oversight — all of which must actively resist executive attempts to reassert discretionary power. Theater is low throughout (0.08 to 0.28) because the constraint's core function is REAL: it genuinely transforms how authority operates. The theater ratio rises slightly over time (from 0.08 to 0.28) because later invocations of Clause 39 become more symbolic as the underlying legal infrastructure solidifies — judges and Parliament need less to DO in defense of the principle because its internalization is advanced. Resistance is HIGH (0.74) because the Crown and feudal authorities ACTIVELY resist this constraint throughout the period; they contest its reach, invoke emergency exceptions, and seek to narrow its application. The constraint persists not by participant preference but by force of legal interpretation, repeated enforcement, and institutional investment in constitutionalism.
 *
 * PERSPECTIVAL GAP:
 *   The executive power seat and the beneficiary seats (individual subjects, Parliament) compute RADICALLY differently. From the Crown's seat, the constraint is a severe extraction: it loses the ability to act on discretionary will and must go through legal process. From the subject's seat, it is protection and coordination: the law itself becomes a weapon against arbitrary power. From Parliament's seat, it is both — Parliament is elevated to a co-guardian of the constraint, which increases its authority over the Crown. From the feudal lord's seat, it is pure extraction without compensation: the right to fine and imprison at will, a feudal prerogative, is stripped away. The engine computes these per-seat divergences from the structural data — who benefits, who bears costs, what their exit options are. The authored claim (Tangled Rope) and the authored metrics (high extractiveness, high suppression) are independent; the claim is what the reading ASSERTS the constraint is ABOUT, while the metrics are what the constraint DOES in operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (individual_subjects_protected) and victim declarations (executive_power_constrained, feudal_arbitrators_dispossessed) map directly to structural extraction. Individual subjects are beneficiaries because they gain a legal shield against arbitrary power with no exit cost — the constraint is FOR them. The executive and feudal actors are victims because the constraint strips them of discretionary authority; they pay the cost of legal process and the loss of summary justice. The directionality derivation chain: beneficiaries → d near 0 (subsidy/protection flow); victims → d near 1.0 (extraction target). No directionality overrides are needed; the structural data is clean. However, Parliament and common-law jurists are BOTH beneficiaries (they gain authority and professional status) and yet also quasi-payers (they must continuously enforce the constraint and interpret it). In the schema, Parliament is role=agenda_setter (it sets and enforces the legal standard), which derives d somewhere in the middle — not pure target, not pure beneficiary, but a co-ruler of the constraint. Common-law jurists are dual-roled: beneficiary (they profit from the profession the constraint creates) and agenda_setter (they are the interpreters). This dual positioning reflects their actual structural seat: they are elevated by the constraint but also bound by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was LIVE in 1215 (arbitrary power running rampant) and remains LIVE today (executives still seek to exempt themselves from legal constraint; we see this in invocations of state-of-exception, national security, and executive privilege in every generation). The world-would-rearrange verdict is SECURE: if Clause 39 and the centuries of legal development it anchors disappeared, constitutional governance would evaporate and executive discretion would be unchecked. Therefore, there is NO mandatrophy in the strict sense — the founding problem has not been solved such that the constraint persists from inertia. Rather, the constraint persists because the founding problem is PERENNIAL: every generation of executives seeks to expand discretionary power, and every generation must re-invoke Clause 39 and its successors (Fifth Amendment, Fourteenth Amendment, human-rights instruments) to reassert the principle. The theater ratio is LOW (0.28 at t800) because the constraint is doing REAL WORK: it genuinely shapes how authority operates. If the constraint had become purely theatrical — if courts were issuing rulings affirming due-process rights while executives were acting arbitrarily anyway — the theater ratio would be much higher (0.6+). Instead, the constraint remains functionally operative: executives cannot simply dismiss subjects without due process; they must go through courts; they must justify action in legal terms. The constraint HAS accumulated extraction (0.15 → 0.82 over 800 years), which is expected: as the legal profession deepens, as courts expand interpretation, as Parliament centralizes power, the extraction from competing principles (discretionary executive authority, feudal particularism) increases. This is not mandatrophy; it is the deepening of the constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_discovered_natural_law,
    'Is ''law of the land'' a discovered natural principle that Clause 39 merely articulates, or a reading that medieval and later actors IMPOSE on the text?',
    'Textual analysis of 1215 usage and contemporary glosses; comparison with other medieval charters using the same phrase in narrower contexts; examination of whether the expanded reading emerges from the text or from interpretive tradition (common-law reasoning, statute, judicial precedent).',
    'If discovered: the constraint''s legitimacy rests on natural law and the principle is independent of political will — makes the constraint more durable against executive override. If reading-imposed: the constraint''s durability depends on institutional maintenance and interpretive tradition — makes it vulnerable to paradigm shifts in legal philosophy or institutional collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_discovered_natural_law, conceptual, 'Whether the liberal due-process reading is a timeless principle or a medieval/early-modern construction that later actors mistook for timelessness.').

omega_variable(
    universal_vs_estate_bound,
    'Does Clause 39 protect ALL subjects universally, or only ''free men'' as a feudal estate distinct from villains and unfree persons?',
    'Historical record of application: did medieval courts extend Clause 39 protections to serfs and unfree persons, or only to the free estate? When and how did the reading expand to universal coverage? Was the expansion a reinterpretation of Clause 39 or a replacement via statute (e.g., abolition of villeinage)?',
    'If originally estate-bound: the liberal universality is a later addition, not inherent in Clause 39, and represents a radical expansion of the reading''s scope. If universalizing was always implicit: the reading is more stable and less contingent on later political choices. Higher impact on the constraint''s true victim set in 1215 vs. 1800.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_estate_bound, empirical, 'The scope of ''person'' protected under the reading has expanded from feudal estates to universal subjects; this scope is contested in the historical record.').

omega_variable(
    feudal_custom_vs_written_law,
    'What counts as ''the law of the land''? Is it customary feudal practice, written statute, or abstract legal principle?',
    'Comparison of how the three readings deploy the phrase: feudal prerogative reading emphasizes custom and hierarchy; originalist reading emphasizes the specific 1215 context; liberal reading emphasizes written law and universal principle. Examine which interpretation has prevailed in common-law courts over time.',
    'If ''law of the land'' means custom, the constraint is conservative and preserves feudal order — the feudal reading wins. If it means written statute, the constraint centralizes authority in Parliament. If it means abstract principle, it becomes a tool for radical expansion. The reading''s force depends entirely on this definitional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_custom_vs_written_law, conceptual, 'The referent of ''law of the land'' is the crux of the contest between the three readings.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the liberal due-process reading FORECLOSE the feudal prerogative reading, or do they coexist as live positions in different seats?',
    'Examine whether any single institutional authority (Crown, Parliament, courts) has adopted the liberal reading such that the feudal reading is logically impossible within that framework, or whether both readings persist as contested principles held by different factions.',
    'If foreclosed: the feudal reading is a dead option and the liberal reading has won institutional authority. If coexisting: both readings remain available as interpretive choices, which means the constraint''s reach is still contested and subject to reversal under different leadership.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether the readings are competing live positions or whether the liberal reading has achieved institutional dominance.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71 at t800) structural (external enforcement machinery required continuously) or internalized (executives and courts have internalized the constraint as a legitimate limit)?',
    'Observe what happens to executive behavior if enforcement machinery is temporarily absent: do executives immediately revert to arbitrary action, or do they continue respecting due-process limits because the principle is internalized? Monitor cases where executives invoke state-of-exception or national-security exceptions and whether courts accept or reject the claim.',
    'If structural: the constraint requires continuous active enforcement and collapses if enforcement lapses. If internalized: the constraint is self-maintaining and survives enforcement lapses. The high suppression value (0.71) suggests structural — but internalization may be higher than suppression indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of counter-principles is active/structural or passive/internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t400, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 400, 0.24).
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t600, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 600, 0.27).
narrative_ontology:measurement(constraint_magna_carta_clause_39_theater_t800, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.28).

% Extraction over time
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t400, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t600, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 600, 0.76).
narrative_ontology:measurement(constraint_magna_carta_clause_39_extractiveness_t800, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t400, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 400, 0.66).
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t600, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 600, 0.69).
narrative_ontology:measurement(constraint_magna_carta_clause_39_suppression_t800, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 800, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Clause 39 of Magna Carta is a contested kernel with three distinct readings that constitute three separate constraints. The liberal due-process reading (this story) expands Clause 39 to a universal principle; the feudal prerogative reading (sibling) preserves it as a feudal privilege; the originalist limitation reading (sibling) restricts it to 1215-documented abuses. Each reading carries different beneficiary/victim sets, different ε values, and different terminal types. They are linked via network.affects_constraints because they share an interpretive tradition: the expansion of the liberal reading pushes back against the feudal reading, and the originalist reading constrains how far the liberal expansion can go. The three readings form a constraint family, all descendants of the same kernel text, competing for institutional authority across eight centuries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
