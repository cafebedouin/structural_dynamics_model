% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right: Magistrate Constitutional Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The remonstrance right, as read through the magistrate lens, positions
 *   the Parlement courts as constitutional guardians preserving ancient
 *   liberties against arbitrary royal innovation. When the crown issues a
 *   fiscal edict (new tax, exemption removal, judicial reorganization), the
 *   magistrates remonstrate—formally protesting and delaying registration in
 *   provincial courts, which prevents the edict's enforcement. The magistrate
 *   reading frames remonstrance as a fundamental check on power; the crown
 *   reading frames it as minoritarian veto protecting tax-exempt privileges.
 *   This constraint story instantiates the magistrate reading's structural
 *   claim: that remonstrance coordinates a constitutional principle (no
 *   arbitrary innovation) while extracting fiscal benefit for the magistrate
 *   class and shifting burden to commoners. The measurement series tracks how
 *   extractiveness and suppression requirement both intensified over the
 *   interval as remonstrance became more theatrical (defense of principle)
 *   while functioning more as class protection (blocking broad reforms that
 *   would tax the nobility).
 *
 * KEY AGENTS:
 *   - robe_nobility_magistracy: agenda-setter and primary beneficiary (institutional power, constrained exit) — frames and executes remonstrance; exempt from taxes they block
 *   - provincial_parlements: beneficiary and secondary agenda-setter (organized power, constrained exit) — collectively wield remonstrance as veto; depend on the mechanism's legitimacy
 *   - crown_fiscal_authority: primary payer (powerful, constrained exit) — blocked from fiscal reforms; must negotiate with magistrates or abandon edicts
 *   - subject_taxpayers: victim (powerless, trapped exit) — bear redirected tax burden when magistrates block broad reforms
 *   - reform_oriented_ministers: excluded (powerful, constrained exit) — blocked by remonstrance framework; cannot propose or implement fiscal modernization
 *   - constitutional_observer: analytical seat — documents the constraint's operation and mandatrophy trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.76).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right: Magistrate Constitutional Reading").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/political").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '61dccbf5-8688-4802-b534-ea66d913a86e').
narrative_ontology:cs_kernel_codification('61dccbf5-8688-4802-b534-ea66d913a86e', fixed_text).
narrative_ontology:cs_authority_grounding('61dccbf5-8688-4802-b534-ea66d913a86e', lineage).
narrative_ontology:cs_interpretation_layer_present('61dccbf5-8688-4802-b534-ea66d913a86e').
narrative_ontology:cs_reading_relation('61dccbf5-8688-4802-b534-ea66d913a86e', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('61dccbf5-8688-4802-b534-ea66d913a86e', foundational, ancient_constitutional_continuity_preserves_law).
narrative_ontology:cs_axiom_status(ancient_constitutional_continuity_preserves_law, holdable).
narrative_ontology:cs_axiom_grounding('61dccbf5-8688-4802-b534-ea66d913a86e', ancient_constitutional_continuity_preserves_law, deontological).
narrative_ontology:cs_axiom('61dccbf5-8688-4802-b534-ea66d913a86e', foundational, magistrate_veto_prevents_arbitrary_innovation).
narrative_ontology:cs_axiom_status(magistrate_veto_prevents_arbitrary_innovation, holdable).
narrative_ontology:cs_axiom_grounding('61dccbf5-8688-4802-b534-ea66d913a86e', magistrate_veto_prevents_arbitrary_innovation, instrumental).
narrative_ontology:cs_reference_frame('61dccbf5-8688-4802-b534-ea66d913a86e', immemorial_magistrate_constitutional_authority).
narrative_ontology:cs_drift_state('61dccbf5-8688-4802-b534-ea66d913a86e', late_eighteenth_century, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('61dccbf5-8688-4802-b534-ea66d913a86e', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, robe_nobility_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_parlements).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, subject_taxpayers_bearing_redirected_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The magistrate class (robe nobility, holders of venal offices in Parlements and other courts) claim and exercise remonstrance authority—the right to formally protest and delay royal edicts before they are registered in provincial courts. They frame this as defense of ancient constitutional liberties and customary law against arbitrary royal innovation. They benefit materially: they can block edicts that would tax their estates, reduce their judicial prerogatives, or centralize authority in Paris. They benefit institutionally: remonstrance gives them a formal veto over legislation affecting their domains. Their identity is constituted through the magistrate role—being a defender of law against power is central to how they see themselves. Exit would mean surrendering this role entirely.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, robe_nobility_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, robe_nobility_magistracy, beneficiary).

% The regional Parlement courts (particularly Paris, but also provincial ones) collectively wield remonstrance authority and use it to block edicts that would centralize power, impose new taxes on their regions, or diminish their judicial independence. Each Parlement's magistrates benefit from being able to frame regional resistance as constitutional duty. Collectively, the Parlements can bring fiscal reform to a halt. Individually, each Parlement depends on the constraint's legitimacy—if remonstrance is exposed as pure privilege protection, their authority to act evaporates and they face the label 'traitor' rather than 'constitutional guardian.'
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_parlements, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, provincial_parlements, agenda_setter).

% The crown (king, treasury ministers, fiscal advisors) seek to raise revenue through edicts—new taxes, elimination of feudal exemptions, consolidation of fiscal administration. Remonstrance delays these edicts, forces negotiation with magistrates, requires exempting magistrate property, or leads to abandonment of the reform. The crown cannot simply override the magistrates without risking delegitimization (appearing tyrannical and thereby triggering coordinated resistance that would dissolve the constraint entirely). The crown is trapped: it needs revenue but cannot bypass the constitutional framework that legitimates its own authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_authority, payer,
    powerful, biographical, constrained, national).

% Commoners and non-noble property holders have no remonstrance rights and no voice in fiscal proceedings. When magistrates block a broad fiscal reform (e.g., elimination of feudal dues, consolidation of tax collection), the crown compensates by raising indirect taxes (salt tax, alcohol tax, feudal dues on non-magnates) or imposing new imposts that fall on the poor. They bear the burden of the magistrates' veto; their exits are emigration (expensive, difficult) or tax evasion (legally dangerous). They carry the cost of the remonstrance mechanism but have no representation in it.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, subject_taxpayers_bearing_redirected_burden, payer,
    powerless, biographical, trapped, national).

% Ministers and advisors seeking to modernize the tax system, rationalize feudal exemptions, improve judicial efficiency, or expand state capacity find themselves blocked by remonstrance. They would argue that ancient liberties perpetuate inefficiency, inequality, and international weakness; that constitutional evolution requires overriding particularist veto; that the magistrates are defending privilege, not law. They are structurally excluded from remonstrance proceedings—the magistrates set the terms, the crown negotiates within them, and reform ministers must present their case to magistrates who benefit from blocking it.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, reform_oriented_crown_ministers, excluded,
    powerful, biographical, constrained, national).

% Later constitutional historians, revolutionary-era critics, and contemporary legal scholars examine the remonstrance mechanism's operation and consequences. They document the tension between the magistrates' constitutional language (defense of ancient liberties) and the actual pattern of remonstrance (selective blocking of tax-reform edicts, exemption of magistrate property, burden-shifting to commoners). They assess whether remonstrance preserved genuine constitutional principle or functioned as class-privilege protection under constitutional cover.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_observer, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, robe_nobility_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a formal constitutional checkpoint: royal edicts on major matters (especially fiscal, judicial) must be submitted to provincial magistrate courts for remonstrance before registration and enforcement. The mechanism coordinates the relationship between royal legislative authority and magistrate constitutional authority. Without it, the crown could legislate unilaterally; without magistrate acquiescence, edicts lack legitimacy and practical enforcement in the provinces. The magistrate reading claims this checkpoint preserves rule of law by requiring the crown to justify novel edicts in terms of ancient law and customary authority, rather than arbitrary will.
% TRANSFER_FUNCTION: Moves fiscal burden: the magistrate class and their estates are exempt from taxes they block; the burden transfers to commoners through redirected taxes (salt, alcohol, feudal dues on non-magnates), new imposts, or abandoned reforms that would have rationalized the system. Also moves authority: the magistrates gain formal veto power over royal legislation affecting their domains; the crown loses unilateral legislative initiative on major edicts.
% ABSENT_VOICES: Commoners and non-noble property holders are excluded from remonstrance proceedings; their burden increases when magistrates block broad fiscal reform. Reform-oriented crown ministers are structurally outside the remonstrance framework—the magistrates set the terms of the debate and the crown negotiates within them, not the other way around. International competitors would prefer the constraint persist as a weakness in French fiscal capacity but have no voice in French constitutional proceedings.
% DISAPPEARANCE_RATIONALE: If remonstrance authority vanished overnight, the crown would complete long-blocked fiscal reforms (unified tax system, elimination of feudal exemptions, consolidation of tax administration). The magistrate class would lose their institutional veto and their claim to constitutional authority. The tax system would rationalize and the burden distribution would shift from class-based (magistrates exempt, commoners bearing redirected cost) toward more uniform distribution. Provincial Parlements would lose a major source of institutional power and legitimacy. The constraint's disappearance would require either explicit constitutional amendment (legitimized override of the magistrates' claim) or a revolution that delegitimizes the magistrate class's authority claims entirely (which is historically what occurred in 1789).
% FOUNDING_PROBLEM: In the medieval and early modern periods, regional magnates held autonomous jurisdictional and fiscal authority; the church held vast tax-exempt lands. The founding problem was: how can a centralizing crown consolidate governance and raise revenue without triggering civil war with regional powers (magnates, magistrates, church) who claim their authority is immemorial, divinely ordained, and superior to novel royal claims? Remonstrance was offered as a solution: the magistrates retain judicial authority and a formal voice in the legislative process; the crown gains the ability to legislate; both claim to respect ancient liberties and customary law rather than arbitrary will.
% FOUNDING_PROBLEM_CORROBORATION: The magistrate reading attests the founding problem is live and persistent: constitutional government requires that those who hold power respect ancient liberties and submit novel edicts to existing law; remonstrance IS the mechanism that enforces this constitutional principle and prevents arbitrary innovation. The crown reading attests the founding problem is substantially dead: by the 18th century, fiscal consolidation has largely occurred despite remonstrance delays; the mechanism now primarily protects magistrate tax exemptions rather than general constitutional principle. Contemporary historians outside the magistrate class (Turgot, economists, revolutionary-era critics) attest the founding problem has shifted: the original problem (regional magnate autonomy, church exemptions, fragmented authority) has been substantially addressed; the new problem is that remonstrance blocks rational fiscal reform under the guise of constitutional principle, perpetuating inequality and international weakness. No corroboration from outside the benefiting magistrate class supports the claim that remonstrance's function remains constitutional principle-preservation rather than privilege-protection.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.42 at t0) because the coordination function is genuine—remonstrance does establish a constitutional checkpoint and prevent unilateral crown action. By t250 (0.68) extractiveness rises substantially as the magistrate class uses the mechanism increasingly to protect fiscal privileges rather than defend general constitutional principle; the measurement series captures the drift from genuine constitutional coordination toward class-interest protection. Suppression requirement rises from 0.52 to 0.76 because maintaining remonstrance's legitimacy becomes harder as the gap widens between the stated principle (preserving ancient liberties) and the actual function (protecting tax exemptions). Theater ratio tracks this delegitimization: at t0 remonstrance genuinely coordinates law-above-power; by t250 remonstrance becomes increasingly theatrical—the magistrates must frame every block as constitutional duty while commoners observe the outcome is tax exemption. The measurement grid tracks a single shared time axis (interval 0–250) so every metric is authored at every examined point; the rising theater ratio alongside rising extractiveness models a Goodhart drift (the stated goal—constitutional principle—decouples from the measured function—privilege protection).
 *
 * PERSPECTIVAL GAP:
 *   The magistrate seat (robe_nobility) experiences remonstrance as constitutional necessity—the mechanism that prevents tyranny and preserves rule of law. The crown seat experiences it as costly veto blocking urgent reforms. The commoner/taxpayer seats experience it as a mechanism by which the privileged block broad reforms and shift burden downward. The engine computes each seat's classification from the structural data: the magistrate seat gets low d (beneficiary, powerful, identity-locked in the magistrate role) and may classify the constraint as rope or tangled rope; the crown seat gets high d (target, constrained exit, forced negotiation) and likely computes tangled rope or snare; the powerless taxpayer seats get high d (trapped, bearing burden) and likely compute snare. The magistrate reading's claimed type (tangled_rope) sits between the extremes—it acknowledges both coordination (the checkpoint function) and asymmetric extraction (the class benefit), but asserts the coordination is primary. The engine's per-seat computation will likely show divergence: magistrate seats compute rope or tangled rope, crown and commoner seats compute tangled rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The robe_nobility_magistracy are clear beneficiaries (d near 0.0–0.25): they set the terms, are exempt from the taxes they block, hold institutional power, and their identity is constituted through the magistrate role (identity-locked exit). The crown is a target (d near 0.75–1.0): constrained exit (must negotiate or delegate), powerful but forced into defensive negotiation, and blocked from its stated fiscal goals. Subject taxpayers are full targets (d = 1.0): powerless, trapped, bearing the burden of redirected taxation. Provincial Parlements are partial beneficiaries (d near 0.3–0.5): they benefit from the veto authority but depend on the constraint's legitimacy—if remonstrance is exposed as pure privilege protection, their authority crumbles. This directionality structure makes the constraint tangled rope from the magistrate seat (coordination + asymmetric extraction) and snare from the commoner seat (pure extraction via institutional veto). The overrides are not needed because the beneficiary/victim declarations and power/exit atoms produce the correct derivation: beneficiaries are institutional and constrained (low d); victims are powerless or powerful-but-blocked (high d). The magistrate reading's claimed type (tangled rope) accurately reflects the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The magistrate reading asserts the founding problem (constitutional continuity against arbitrary innovation) remains live and remonstrance is the mechanism that preserves it. The measurement data and the crown-reading challenge both contest this. If the founding problem is indeed dead (fiscal consolidation, centralization, and modernization have largely occurred despite remonstrance's delays) and remonstrance persists, the constraint exhibits mandatrophy: it maintains a constitutional language (defense of ancient liberties) while functioning as pure privilege protection (tax exemption for the magistrate class). The theater_ratio rising to 0.42 by t250 models this mandatrophy: the constraint requires increasing theatrical maintenance (repeated invocation of constitutional duty) to sustain legitimacy even as the actual function (blocking broad fiscal reform to protect privilege) becomes increasingly visible. The magistrate reading claims the founding problem is live; the measurements suggest the theater-to-function ratio indicates it is dead or dying. This mismatch is precisely the kind of mandatrophy that the six_questions and founding_problem_status fields are designed to surface. The magistrate reading's own data (the rising theater ratio, the concentrated beneficiary class, the powerless victim set) provides evidence for mandatrophy; the reading does not deny or minimize this—it stakes the constraint on the assertion that the founding problem remains live, and omega variables (below) capture the contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'By the 18th century, is the founding problem—protection of ancient liberties against arbitrary royal innovation—genuinely live, or has it been displaced by a different constraint (fiscal privilege protection)?',
    'Examine the outcomes of blocked remonstrance edicts: do they preserve general constitutional principles (rule of law, customary authority) or do they disproportionately protect magistrate fiscal exemptions? Compare magistrate remonstrance patterns: do they oppose all edicts equally (consistent constitutional principle) or do they selectively oppose tax-reform edicts while accepting others (selective privilege protection)?',
    'If the founding problem is dead and the constraint now functions as privilege protection, the classification shifts from tangled_rope (genuine coordination + asymmetric extraction) to snare (pure extraction via institutional veto). The mandatrophy flag would fire: founding_problem_status=dead + disappearance_verdict=world_rearranges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether remonstrance preserves a live constitutional principle or operates as dead-letter cover for class privilege.').

omega_variable(
    crown_reading_validity,
    'Does the crown reading—that remonstrance is illegitimate minoritarian veto—logically foreclose the magistrate reading, or do both remain live constitutional claims?',
    'Analyze the axioms: if the crown reading stakes on ''remonstrance violates royal legislative authority (empirically_contingent: evidence would show the crown''s authority is ancient and immemorial)'' and the magistrate reading stakes on ''constitutional continuity requires remonstrance (deontological: this is a duty, not an empirical fact)'', neither can foreclose the other absent agreement on what counts as constitutional authority itself. If both stake on empirical claims about immemorial authority, they can potentially foreclose each other.',
    'If the readings foreclose rather than coexist, the constraint story collapses into a single kernel-reading contest (one reading''s axioms logically eliminate the other); if they coexist, both remain live and the kernel persists as genuinely contested. This affects the narrative frame: is this story one reading of an ongoing constitutional dispute (coexist_with) or a claim that ultimately prevails and eliminates the alternative (forecloses)?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_reading_validity, conceptual, 'Whether the crown and magistrate readings occupy logically incompatible frameworks or genuinely coexist as live constitutional claims.').

omega_variable(
    commoner_burden_quantification,
    'How much of the tax burden redirected to commoners is attributable to blocked magistrate remonstrance, versus other crown revenue-raising mechanisms?',
    'Fiscal analysis comparing tax composition before and after major remonstrance blocks: do redirected taxes (salt, alcohol, feudal dues) rise proportionally to blocked broad-reform edicts? Track which tax categories show largest increases following magistrate obstructions.',
    'If commoner burden rises proportionally to magistrate blocking, the extraction is clear (d near 1.0 for powerless taxpayers, snare-class dynamics). If burden rises primarily from other crown decisions, the extraction is more diffuse and the class asymmetry is lower than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commoner_burden_quantification, empirical, 'Whether magistrate remonstrance directly causes redirected burden to commoners or the burden shift has other primary causes.').

omega_variable(
    identity_lock_magistrate_class,
    'Is the magistrate class''s commitment to remonstrance defense identity-locked (the role IS their self-concept) or pragmatically locked (they benefit materially but could abandon it without losing identity)?',
    'Historical analysis of magistrate discourse: do they justify remonstrance via constitutional principle (identity-locked language: ''we are the guardians of law'') or via interest (pragmatic language: ''this exempts our property'')? Post-revolutionary evidence: do magistrate descendants or their ideological successors continue defending remonstrance if property exemptions are removed?',
    'If identity-locked, the magistrate seat''s exit option is truly identity_locked (d modified accordingly) and the constraint persists even after material benefits erode; if pragmatically locked, exit becomes possible once material benefits diminish or are seized, and the constraint becomes vulnerable to sudden collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_magistrate_class, empirical, 'Whether magistrate commitment to remonstrance is identity-constituted or materially pragmatic.').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint instantiates the magistrate reading of the remonstrance_authority kernel. The crown reading contests it. Are these readings genuinely alternative interpretations of the same kernel (coexist_with), or does one reading''s logical structure foreclose the other''s (forecloses)?',
    'Examine the foundational axioms of each reading (authored in cs_structure for both constraints). If one reading''s foundational axiom is ''the ancient constitution was designed to protect magistrate authority'' (deontological) and the other''s is ''the ancient constitution never granted magistrates veto over royal legislation'' (empirical_contingent), the readings occupy different epistemic grounds and coexist. If both stake on the same empirical fact (e.g., ''did the medieval parliament have veto authority''), one reading''s axiom can be falsified and that reading foreclosed.',
    'If coexist_with, both readings remain live in the 18th-century constitutional debate; the constraint story is ONE valid reading among live alternatives. If forecloses, this reading''s foundational premise logically eliminates the crown reading''s core claim; the constraint story asserts the crown reading is incoherent or empirically false (a stronger but riskier claim).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the magistrate and crown readings coexist as live constitutional claims or one logically forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(remo_tr_t50, remonstrance_authority__magistrate_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(remo_tr_t100, remonstrance_authority__magistrate_reading, theater_ratio, 100, 0.31).
narrative_ontology:measurement(remo_tr_t150, remonstrance_authority__magistrate_reading, theater_ratio, 150, 0.36).
narrative_ontology:measurement(remo_tr_t200, remonstrance_authority__magistrate_reading, theater_ratio, 200, 0.39).
narrative_ontology:measurement(remo_tr_t250, remonstrance_authority__magistrate_reading, theater_ratio, 250, 0.42).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(remo_be_t50, remonstrance_authority__magistrate_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(remo_be_t100, remonstrance_authority__magistrate_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(remo_be_t150, remonstrance_authority__magistrate_reading, base_extractiveness, 150, 0.64).
narrative_ontology:measurement(remo_be_t200, remonstrance_authority__magistrate_reading, base_extractiveness, 200, 0.66).
narrative_ontology:measurement(remo_be_t250, remonstrance_authority__magistrate_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(remo_su_t50, remonstrance_authority__magistrate_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(remo_su_t100, remonstrance_authority__magistrate_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(remo_su_t150, remonstrance_authority__magistrate_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement(remo_su_t200, remonstrance_authority__magistrate_reading, suppression_requirement, 200, 0.74).
narrative_ontology:measurement(remo_su_t250, remonstrance_authority__magistrate_reading, suppression_requirement, 250, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel has two readings: magistrate_reading (this constraint) asserts remonstrance preserves ancient liberties through constitutional checkpoint; crown_reading asserts remonstrance is illegitimate veto protecting tax-exempt privilege. The readings share the kernel (the authority to remonstrate) but diverge on ε and on the beneficiary/victim structure. The magistrate reading carries high ε for fiscal edicts (0.68) and names magistrate class and Parlements as beneficiaries; the crown reading carries different ε (lower) and names the crown and commoners as coordinated against magistrate privilege. The two constraints are linked via network.affects_constraints and document the epistemic and structural contest in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
