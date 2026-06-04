% ============================================================================
% CONSTRAINT STORY: habeas_corpus_act_1679__procedural_teeth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_habeas_corpus_act_1679__procedural_teeth_reading, []).

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
 *   constraint_id: habeas_corpus_act_1679__procedural_teeth_reading
 *   human_readable: Habeas Corpus Act 1679: Procedural Teeth Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The Habeas Corpus Act of 1679, in the 'procedural teeth' reading, is a
 *   constraint that solves the coordination problem: 'How do we make
 *   illegitimate detention legally impossible?' The ancient writ of habeas
 *   corpus existed for centuries before 1679; what the Act adds is not new
 *   substantive rights but new machinery to enforce existing ones.
 *   Specifically: (1) Automatic deadlines for custodians to produce the
 *   detainee and articulate legal grounds (typically 20 days maximum,
 *   adjusted for distance), (2) Personal financial penalties on judges and
 *   gaolers for missed deadlines (£10-30 per day of delay—substantial in the
 *   17th century), (3) A prohibition on re-commitment for the same cause
 *   (preventing the evasion tactic of cycling: detain, release on habeas,
 *   immediately re-detain for a technically different offense). The genius of
 *   the Act is that it is purely procedural: it does not create new
 *   substantive rights or judicial powers; it creates a schedule of
 *   mechanical enforcement that prices the delay tactic and makes indefinite
 *   detention without articulated grounds impossible. This reading
 *   instantiates one interpretation of what habeas corpus is: a set of
 *   enforceable procedures that subordinate discretionary executive detention
 *   to judicial review under time pressure. Two sibling readings exist: the
 *   'modern_detention_tests' reading sees the procedural framework as still
 *   operative in contemporary detention law (Belmarsh, immigration detention,
 *   pre-trial detention without trial); the 'suspension_history' reading
 *   emphasizes that habeas has always lived with its suspensions—the liberty
 *   is as much defined by when Parliament may switch it off as by the writ's
 *   existence.
 *
 * KEY AGENTS:
 *   - Detainees / Persons Unlawfully Confined: Primary beneficiaries (powerless/trapped initially, constrained after 1679) — the machinery creates an enforceable pathway to release or articulation of lawful grounds
 *   - Custodians (Judges, Gaolers, Crown Officers): Institutional actors bearing mixed coordination burden and extraction burden (institutional/constrained) — responsible for meeting deadlines and articulating grounds; subject to penalties for evasion
 *   - Unlawful Custodians (Those Using Detention-by-Delay as Coercion): Institutional actors with extractive intent (institutional/trapped relative to the constraint) — the machinery directly precludes their extraction mechanism
 *   - Parliament (Constitutional Authority): Organized lawmaker (organized/mobile) — enacts the constraint as a scaffolding mechanism to establish rule-of-law norms about detention
 *   - Legitimate Judicial Authority: Institutional actors acting within law (institutional/arbitrage) — experiences the constraint as enabling, not burdensome; benefits from clarity and legitimacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the 1679 Act as a canonical procedural innovation: how to enforce rights through mechanical deadlines and penalties rather than appeals to benevolence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(habeas_corpus_act_1679__procedural_teeth_reading, 0.28).
domain_priors:suppression_score(habeas_corpus_act_1679__procedural_teeth_reading, 0.35).
domain_priors:theater_ratio(habeas_corpus_act_1679__procedural_teeth_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(habeas_corpus_act_1679__procedural_teeth_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__procedural_teeth_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__procedural_teeth_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(habeas_corpus_act_1679__procedural_teeth_reading, rope).
narrative_ontology:human_readable(habeas_corpus_act_1679__procedural_teeth_reading, "Habeas Corpus Act 1679: Procedural Teeth Reading").
narrative_ontology:topic_domain(habeas_corpus_act_1679__procedural_teeth_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(habeas_corpus_act_1679__procedural_teeth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(habeas_corpus_act_1679__procedural_teeth_reading, '8802d4f9-e1bc-43e4-84bf-d016b7084cb5').
narrative_ontology:cs_kernel_codification('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', formalized).
narrative_ontology:cs_authority_grounding('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', lineage).
narrative_ontology:cs_interpretation_layer_present('8802d4f9-e1bc-43e4-84bf-d016b7084cb5').
narrative_ontology:cs_reading_relation('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', habeas_corpus_act_1679__modern_detention_tests_reading, influences).
narrative_ontology:cs_reading_relation('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', habeas_corpus_act_1679__suspension_history_reading, coexists_with).
narrative_ontology:cs_axiom('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', foundational, detention_requires_articulated_legal_grounds).
narrative_ontology:cs_axiom_status(detention_requires_articulated_legal_grounds, holdable).
narrative_ontology:cs_axiom_grounding('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', detention_requires_articulated_legal_grounds, deontological).
narrative_ontology:cs_axiom('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', foundational, evasion_tactics_priced_by_mechanical_penalty).
narrative_ontology:cs_axiom_status(evasion_tactics_priced_by_mechanical_penalty, holdable).
narrative_ontology:cs_axiom_grounding('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', evasion_tactics_priced_by_mechanical_penalty, empirically_contingent).
narrative_ontology:cs_reference_frame('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', judicial_procedural_subordination_of_detention).
narrative_ontology:cs_drift_state('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8802d4f9-e1bc-43e4-84bf-d016b7084cb5', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(habeas_corpus_act_1679__procedural_teeth_reading, habeas_corpus_act_1679).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(habeas_corpus_act_1679__procedural_teeth_reading, detainees).
narrative_ontology:constraint_beneficiary(habeas_corpus_act_1679__procedural_teeth_reading, persons_unlawfully_confined).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DETAINEE (ROPE) — Immediate temporal horizon. The detainee experiences the 1679 Act's machinery as pure coordination mechanism: deadlines force custodians to articulate legal grounds or release within a bounded window (typically 3-20 days depending on distance). Theater is minimal — the procedure is functional, not performative. Exit options are constrained (cannot exit confinement unilaterally, but the machinery creates an exit pathway for unjustified detention). The detainee is the clear beneficiary of the constraint.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CUSTODIAN (TANGLED ROPE) — Biographical temporal horizon. The custodian (judge, gaoler, or crown official) experiences mixed coordination and extraction. The machinery coordinates legitimate detention: specifying grounds, certifying legality, producing evidence. But the machinery also extracts: personal financial penalties for missed deadlines, fines for false imprisonment, no re-commitment after release for the same cause. The custodian bears real cost. Extraction is structural (embedded in the penalty schedule), not avoidable by refusing to detain—the refusal mechanism itself (the deadline) is the extraction mechanism. Suppression is moderate: the custodian can attempt delay, can appeal to higher authority, can argue procedural defects—but the written law closes most evasion routes.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGITIMATE JUDICIAL AUTHORITY (ROPE) — Generational temporal horizon. A judge acting within the constraint's specifications—articulating grounds, meeting deadlines, certifying legality—experiences the 1679 Act as pure coordination. The machinery enables judicial function: it establishes which detentions are lawful by forcing articulation of grounds and public accountability. The judge with good-faith legal authority to detain is not extracted from; the judge benefits from the constraint's clarity and legitimacy. Arbitrage exit (the judge can appeal, can seek higher-court review, can argue their detention was proper) is available. Theater is minimal. This perspective sees a constraint that solves the coordination problem: 'How do we ensure detention is publicly justified?'
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNLAWFUL CUSTODIAN (SNARE) — Immediate temporal horizon. A judge or gaoler deliberately using detention-by-delay as a coercive tool (detaining a political prisoner without legal grounds, hoping the delay will break resistance or exhaust resources before the writ machinery forces release) experiences the 1679 Act as a snare that directly constrains the extraction mechanism. The machinery prevents the unlawful extraction strategy: deadlines foreclose indefinite detention, penalties price the evasion attempt directly, no re-commitment prevents cycling. The custodian is fully trapped—the constraint explicitly targets the unlawful delay mechanism. Suppression is high from the custodian's perspective (no legitimate escape). Extractiveness is high (the penalties are severe, the timing is automatic). This perspective sees the constraint as a structural barrier to unlawful extraction.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENT / CONSTITUTIONAL REFORM (SCAFFOLD) — Generational temporal horizon. Parliament, acting as the organized constitutional authority, enacts the 1679 Act as a scaffolding mechanism for transitioning from discretionary monarchical detention to rule-of-law constraints. The mechanism is temporary in structure: it is enforced not by permanent administrative machinery but by reactive litigation (habeas corpus petitions), private penalties on specific officials, and parliamentary oversight. The sunset clause is implicit: as the norm of lawful procedure becomes internalized and judicial custom reforms, the machinery's dramatic enforcement becomes less needed. Parliament sees the constraint as a tool to establish a new norm (no indefinite detention) that will eventually become self-enforcing through practice. Exit is available (Parliament can repeal; the constraint is written law, not a natural law). Theater is low relative to the functional outcome: the procedure works.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — Civilizational temporal horizon. From the analytical perspective, the 1679 Act is a canonical example of how procedural enforcement creates coordination where substance alone cannot. The ancient writ (habeas corpus) existed for centuries before 1679; what the Act adds is not new rights but new machinery to enforce existing ones: specific deadlines (typically 20 days maximum journey), automatic penalties on judges and gaolers (£10-30 per day of delay—substantial for officials in the 17th century), and a prohibition on re-commitment for the same cause (preventing cycling). The genius is procedural: the constraint solves the coordination problem 'How do we make illegitimate detention legally impossible?' through rules, not appeals to benevolence. Theater is minimal because the procedure is functional—the results are measurable (release or articulation of lawful grounds). This perspective sees a durable coordination mechanism with high extractiveness only for those attempting unlawful detention.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(habeas_corpus_act_1679__procedural_teeth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__procedural_teeth_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(habeas_corpus_act_1679__procedural_teeth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The Act's core function is coordination—establishing a clear procedure that subordinates detention to judicial review under deadline pressure. Extractiveness is moderate because the penalty schedule (£10-30 per day) does create real costs for custodians. However, extractiveness is not high because the constraint applies asymmetrically: it only extracts from custodians attempting evasion (delay tactics); legitimate custodians who articulate grounds and meet deadlines incur no extraction. The extractiveness value reflects the average across all custodians (some trying to evade, most not). Over time (0 to 100 years), extractiveness declines (0.35 to 0.20) because the norm of procedural compliance becomes internalized—by the 19th century, the penalty mechanism is less frequently invoked because custodians have accepted the procedural norm. Suppression (0.35): Moderate. The Act specifies routes for evasion closure (no re-commitment bar, automatic deadlines, personal liability), so suppression is not total. But the evasion routes that remain (appeals to higher authority, arguments about procedural defects, delay in physical movement of detainees across distance) are limited. Theater ratio (0.15): Low. This is critical to distinguishing this reading from the 'suspension_history' reading. The procedural machinery is functional, not performative. The deadline is real; missing it triggers real penalties; the outcomes (release or lawful ground articulation) are measurable. Theater does not rise significantly over time (0.10 to 0.15) because the function remains stable—the procedure continues to deliver what it promises.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a classic perspectival gap between the beneficiary (detainee) and the custodian. The detainee sees pure coordination (the machinery enables escape from unlawful detention). The legitimate custodian also sees coordination (the machinery clarifies what constitutes lawful detention). The unlawful custodian sees a snare (the machinery precludes their extraction tactic entirely). The custodian bearing legitimate burdens (meeting deadlines, articulating grounds) sees tangled rope (coordination value plus extraction cost). Parliament sees a temporary scaffolding mechanism—the constraint establishes a norm that, once internalized, requires less active enforcement. The analytical observer risks seeing a natural law ('all detention must be time-bounded') but structural data confirms this is a designed constraint: other legal systems have adopted different procedures (indefinite detention, administrative review instead of habeas, no penalty schedule), proving the 1679 machinery is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the beneficiary/victim structure and exit options. Detainees are beneficiaries with constrained exit (cannot exit confinement unilaterally; can only exit if the machinery releases them), yielding low d (d ≈ 0.20-0.25), which produces negative or low effective extraction. Legitimate custodians are neither clear beneficiaries nor victims—they experience the constraint as coordination, with arbitrage exit (higher courts, procedural appeals), yielding mid-range d (d ≈ 0.45-0.50). Unlawful custodians attempting evasion are victims of the constraint, with trapped exit (the machinery forecloses their intended extraction tactic), yielding high d (d ≈ 0.80-0.85), producing high effective extraction relative to those custodians. Parliament as the authority enacting the constraint derives d from its institutional position (d ≈ 0.35), reflecting its role as the coordinating authority rather than beneficiary or victim. The perspective tuple automatically computes d from these structural positions; the commentary documents the logic but does not require explicit arithmetic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_versus_substantive_habeas,
    'Does the 1679 Act''s procedural machinery (deadlines, penalties, re-commitment bar) constitute the core reading of habeas corpus, or is habeas defined primarily by the substantive right to challenge detention''s legality?',
    'Historical analysis of case outcomes before and after 1679; comparison of detainees'' outcomes under the writ alone (pre-1679) versus under the 1679 Act machinery; examination of whether repeals or suspensions of the Act were followed by effective alternative procedural mechanisms.',
    'If procedural machinery is core: the ''modern detention tests'' reading''s claim that the 17th-century machinery remains live in contemporary detention law (Belmarsh, immigration detention) is structurally sound. If substantive right is core: the procedural reading may misidentify habeas as primarily a mechanical constraint rather than a rights-based one, and modern readings may instantiate a different constraint entirely (about detention legality, not deadline enforcement). Affects classification of sibling readings and their relative authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_versus_substantive_habeas, conceptual, 'Whether habeas is defined by procedural machinery or substantive right to challenge').

omega_variable(
    penalty_effectiveness_historical,
    'Were the financial penalties (£10-30 per day of delay) on judges and gaolers in the 1679 Act actually paid and enforced, or were they largely symbolic?',
    'Archival study of 17th and 18th century court records; instances where penalties were actually levied against custodians for breach; correlation between penalty rate and incidence of delay tactics; comparative analysis with pre-1679 enforcement mechanisms.',
    'If penalties were consistently enforced: the procedural reading''s extractiveness value (0.28) is accurate and the rope classification is sound—the constraint genuinely priced evasion. If penalties were rarely collected: extractiveness was lower (closer to 0.10-0.15) and the constraint was closer to pure coordination (stronger rope), or penalties were theater (closer to piton) and extractiveness of delay tactics remained high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penalty_effectiveness_historical, empirical, 'Whether statutory penalties were historically enforced or largely symbolic').

omega_variable(
    suspension_versus_core_identity,
    'When Parliament suspended habeas (during rebellion, civil war, wartime), does the suspension constitute a reading of habeas (the ''suspension_history'' reading) or does it represent a breakdown of the habeas system entirely, distinct from the constraint structure?',
    'Textual analysis of suspension acts: do they position themselves as temporary exceptions to habeas, or as a separate legislative regime? Analysis of constitutional theory: did 17th-18th century jurists view suspension as modifying habeas or as replacing it? Post-suspension outcomes: was the suspended right explicitly restored, or was it restored implicitly through absence of explicit suspension?',
    'If suspension is a reading: then habeas corpus act 1679 includes the possibility of being turned off—the constraint''s identity is partly constituted by the procedures for its own suspension. The ''procedural_teeth'' reading claims deadlines and penalties are the genius; the ''suspension_history'' reading claims the capacity to suspend is equally structural. If suspension is outside the constraint: then the three readings do not exhaust habeas corpus act 1679—they represent different aspects of a larger system. Affects whether the readings coexist or one forecloses another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_versus_core_identity, conceptual, 'Whether suspension of habeas is internal or external to the constraint''s identity').

omega_variable(
    modern_detention_scope_ambiguity,
    'Do the modern detention tests (immigration detention, national security detention, pre-trial detention without trial) represent the same constraint as the 1679 procedural machinery, or do they instantiate a different constraint with its own procedure and penalty structure?',
    'Doctrinal analysis: do modern detention regimes cite or rely upon the 1679 Act''s procedural framework, or do they invoke separate statutory and common-law schemes (e.g., immigration detention under separate statute with different timelines)? Case law: in cases like Belmarsh, did courts apply 1679-Act reasoning or independent human-rights reasoning? Institutional continuity: does the habeas corpus petitioner follow the same procedural path (judge, articulation of grounds, deadline) or a separate path (administrative tribunal, appeal, Human Rights Act)?',
    'If modern detention tests instantiate the same 1679 constraint: the constraint is durable and still functional—the procedural reading remains structurally sound across centuries. If modern detention tests are a separate constraint (with different procedure, different penalty structure, different beneficiary/victim dynamics): then the ''modern detention tests'' reading may misidentify itself as a reading of 1679 when it is actually a new constraint, and the sibling readings need re-scoping. Affects whether the three readings coexist or represent different historical periods of a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_detention_scope_ambiguity, conceptual, 'Whether modern detention regimes continue or replace the 1679 procedural framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(habeas_corpus_act_1679__procedural_teeth_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(habeas_proc_theater_enactment, habeas_corpus_act_1679__procedural_teeth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(habeas_proc_theater_mid_18th, habeas_corpus_act_1679__procedural_teeth_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(habeas_proc_theater_19th, habeas_corpus_act_1679__procedural_teeth_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(habeas_proc_extractiveness_enactment, habeas_corpus_act_1679__procedural_teeth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(habeas_proc_extractiveness_mid_18th, habeas_corpus_act_1679__procedural_teeth_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(habeas_proc_extractiveness_19th, habeas_corpus_act_1679__procedural_teeth_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(habeas_proc_suppression_enactment, habeas_corpus_act_1679__procedural_teeth_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(habeas_proc_suppression_mid_18th, habeas_corpus_act_1679__procedural_teeth_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(habeas_proc_suppression_19th, habeas_corpus_act_1679__procedural_teeth_reading, suppression_requirement, 100, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(habeas_corpus_act_1679__procedural_teeth_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__procedural_teeth_reading, habeas_corpus_act_1679__modern_detention_tests_reading).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__procedural_teeth_reading, habeas_corpus_act_1679__suspension_history_reading).

% DUAL FORMULATION NOTE:
% The 1679 Act kernel admits three distinct constraint readings with different ε values and structural properties. The 'procedural_teeth_reading' (this story) focuses on the mechanical enforcement (deadlines, penalties, re-commitment bar) and yields moderate extractiveness (0.28) for the coordination function. The 'modern_detention_tests' reading examines whether the same procedural framework applies to contemporary detention regimes, potentially yielding different extractiveness values depending on whether modern detention law follows or diverges from the 1679 framework. The 'suspension_history' reading emphasizes that the liberty defined by habeas includes the procedures for its own suspension, which may yield different structural properties (e.g., higher suppression reflecting Parliament's power to remove the constraint). All three readings share the same kernel (the statutory text and doctrinal tradition of 1679) but instantiate different constraints via different interpretive framings. Each story has its own perspectives, measurements, and classification. They are linked via network.affects_constraints to indicate the kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
