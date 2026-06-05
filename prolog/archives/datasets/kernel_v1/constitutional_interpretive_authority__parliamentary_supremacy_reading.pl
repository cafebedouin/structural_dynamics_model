% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/jurisprudence/political_theory
 *
 * SUMMARY:
 *   Parliamentary supremacy — the doctrine that elected legislatures possess
 *   final authority to interpret the constitution, with no power in courts to
 *   void parliamentary acts — represents one reading of a deeply contested
 *   kernel: who decides what the constitution means? This constraint
 *   instantiates the parliamentary supremacy reading specifically, treating
 *   it as a structurally coherent institutional arrangement with its own ε,
 *   beneficiary/victim structure, and perspectival classifications. The
 *   reading is live in Westminster systems (UK, Australia, Canada, New
 *   Zealand) and in theoretical jurisprudence claiming democratic legitimacy
 *   requires legislative supremacy. The constraint exhibits Tangled Rope
 *   structure: genuine coordination function (stable rules for legislation,
 *   predictable outcomes, electoral accountability) coexists with asymmetric
 *   extraction (majorities override minorities, rights claimants lack
 *   judicial recourse, institutional power concentrates in parliament). The
 *   theater ratio (0.64) reflects the role of courts as validators of
 *   parliamentary will rather than constraint mechanisms — judicial review
 *   becomes performative when courts cannot strike down legislation.
 *   Measurements track extraction accumulation and theater growth over time
 *   (likely reflecting 20th–21st century majoritarian expansion and
 *   simultaneous rise of alternative institutional models like judicial
 *   review).
 *
 * KEY AGENTS:
 *   - Elected Legislature: Primary beneficiary (institutional/arbitrage) — exercises interpretive monopoly; coordinates legislative action; captures extractive benefit through majoritarian override
 *   - Electoral Majority: Secondary beneficiary (moderate/organized) — benefits from legislature's unconstrained authority to implement majority preferences
 *   - Constitutional Minority: Primary victim (powerless/trapped) — no judicial override; structurally vulnerable to majoritarian extraction; bears costs of rights violations without recourse
 *   - Individual Rights Claimants: Secondary victim (moderate/constrained) — can petition parliament but face high costs; constrained exit through political mobilization
 *   - Judiciary: Beneficiary-with-extraction (institutional/constrained) — gains interpretive authority but within legislative supremacy; legitimizes parliamentary will; serves theater function
 *   - Constitutional Reform Movement: Organized agents (organized/constrained) — advocate limits on parliamentary supremacy (entrenched rights, judicial authority); see parliament as degraded authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.68).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/jurisprudence/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '799c51b0-0b7a-4455-aaa3-d873ae58c007').
narrative_ontology:cs_kernel_codification('799c51b0-0b7a-4455-aaa3-d873ae58c007', formalized).
narrative_ontology:cs_authority_grounding('799c51b0-0b7a-4455-aaa3-d873ae58c007', extraction).
narrative_ontology:cs_interpretation_layer_present('799c51b0-0b7a-4455-aaa3-d873ae58c007').
narrative_ontology:cs_reading_relation('799c51b0-0b7a-4455-aaa3-d873ae58c007', judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('799c51b0-0b7a-4455-aaa3-d873ae58c007', coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('799c51b0-0b7a-4455-aaa3-d873ae58c007', foundational, electoral_mandate_grants_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_mandate_grants_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('799c51b0-0b7a-4455-aaa3-d873ae58c007', electoral_mandate_grants_interpretive_authority, deontological).
narrative_ontology:cs_axiom('799c51b0-0b7a-4455-aaa3-d873ae58c007', foundational, courts_lack_authority_to_void_legislation).
narrative_ontology:cs_axiom_status(courts_lack_authority_to_void_legislation, holdable).
narrative_ontology:cs_axiom_grounding('799c51b0-0b7a-4455-aaa3-d873ae58c007', courts_lack_authority_to_void_legislation, deontological).
narrative_ontology:cs_axiom('799c51b0-0b7a-4455-aaa3-d873ae58c007', secondary, majoritarian_will_justifies_rights_override).
narrative_ontology:cs_axiom_status(majoritarian_will_justifies_rights_override, overridden).
narrative_ontology:cs_axiom_grounding('799c51b0-0b7a-4455-aaa3-d873ae58c007', majoritarian_will_justifies_rights_override, empirically_contingent).
narrative_ontology:cs_reference_frame('799c51b0-0b7a-4455-aaa3-d873ae58c007', parliamentary_supreme_interpreter).
narrative_ontology:cs_drift_state('799c51b0-0b7a-4455-aaa3-d873ae58c007', contemporary_post_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('799c51b0-0b7a-4455-aaa3-d873ae58c007', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_majority).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minority_rights).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary_institutional_authority).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL MINORITY (SNARE) — Structurally trapped. No exit from legislative majorities; no judicial override available to protect rights. Bear maximum extraction through majoritarian will without recourse. Parliament's interpretive monopoly means legislative acts define rights ex post facto.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL RIGHTS CLAIMANTS (TANGLED ROPE) — Constrained but not fully trapped. Can petition parliament for remedy; can organize politically; can appeal to public conscience. But costs are high: political mobilization, reputational risk, uncertain remedy. Constraint provides coordination through stable legal rules (legislature sets binding interpretation) but extraction through majoritarian override of individual claims.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELECTED LEGISLATURE (ROPE) — Primary beneficiary. Controls interpretive authority through electoral mandate. Experiences the constraint as coordination: stable rules for legislative action, predictable supremacy, ability to bind courts. Net beneficiary through arbitage option (can reinterpret or override judicial interpretation via legislation).
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY (TANGLED ROPE) — Constrained beneficiary with delegated authority. Coordinates common law development and statutory interpretation but cannot override legislative supremacy. Benefits from interpretive role but faces structural extraction: serves as theater for parliamentary will, legitimizes majoritarian decisions, bears reputational cost when precedent contradicts new legislation.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (PITON) — Organized agents advocating limits on parliamentary supremacy (written bills of rights, entrenched judicial authority, constitutional courts). See parliamentary supremacy as degraded — originally functional (protected common law against royal prerogative) but now theater maintaining majoritarian dominance after alternative institutional structures have emerged. Exit path visible but constrained by constitutional amendment difficulty.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some form of ultimate interpretive authority must reside somewhere (logical necessity: someone resolves constitutional disputes). Parliamentary supremacy appears natural — legislatures embody electoral legitimacy, which grounds democratic authority. This perspective risks naturalizing a contingent institutional choice as a logical imperative. FALSE SUMMIT CANDIDATE: engine will detect beneficiaries (legislature, electoral majority) and trigger FSM evaluation.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__parliamentary_supremacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The legislature captures significant extractive benefit through interpretive monopoly — it can redefine rights, override precedent, bind courts. But extraction is not maximal (snare-level 0.66+) because: coordination function is genuine (stable rules, predictable outcomes enable governance), electoral accountability provides some constraint (electoral defeat removes extractive beneficiary), and alternative institutional structures exist (constitutional courts, bills of rights) that demonstrate the arrangement is contingent. The measurement trajectory (0.35 → 0.52) reflects likely historical expansion of legislative scope and simultaneous emergence of competing institutional models. Suppression (0.68): High. Significant barriers to minority exit: no judicial override, majoritarian veto, constitutional amendment difficulty (requires supermajority often including majority consent), high costs of political mobilization. But suppression is not total — exit is theoretically possible through constitutional reform or electoral reversal. Theater ratio (0.64): Moderately high. Courts maintain legitimating function (judicial review, statutory interpretation) but within the constraint of parliamentary supremacy — the interpretive drama performs legitimacy for legislative will rather than constraining it. Rising trajectory reflects growth of judicial theater as alternative institutions (constitutional courts elsewhere) demonstrate courts can play stronger roles.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the full range of perspectival divergence. The legislature sees Rope — coordination mechanism enabling governance, electoral mandate legitimizing authority, no conflict between their interests and constitutional function. The beneficiary sees Rope — their preferences implemented through legislature. The minority sees Snare — trapped without recourse, maximum extraction, no exit. The judiciary sees Tangled Rope — coordination role (interpret law, develop doctrine) mixed with extraction (must validate parliamentary will, reputational cost when precedent overridden, institutional subordination). The reform movement sees Piton — degraded authority maintained through theater despite better alternatives existing. The analytical observer risks Mountain — naturalization of 'someone must have supreme authority' into 'parliament logically must have it' — but structural data reveals beneficiaries and contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to the constraint. Legislature as beneficiary with arbitrage exit: d ≈ 0.15 (low extraction toward them; can escape by supporting constitutional reform or electoral reversal without cost). Electoral majority as secondary beneficiary: d ≈ 0.20. Constitutional minority as trapped victim: d ≈ 0.92 (high extraction; no exit option). Judiciary as institutional constrained beneficiary: d ≈ 0.60 (mixed — benefits from interpretive role but constrained by subordination; cannot exit without constitutional reform). These d values feed the sigmoid f(d) to produce experienced χ (effective extractiveness). The beneficiary perspective (legislature, institutional, arbitrage) produces low χ and classifies as Rope. The victim perspective (powerless, trapped) produces high χ and classifies as Snare. The mixed institutional perspectives (judiciary, organized reform) produce moderate χ and classify as Tangled Rope or Piton depending on time horizon and perception of degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's classification as Tangled Rope is stable across the bases: it genuinely coordinates legislative action (beneficiary sees Rope genuinely) while extracting from minorities (victim sees Snare genuinely). The Piton perspective (reform movement) is not mandatrophy failure — it is a coherent observation that the degraded alternative (parliamentary supremacy now that constitutional courts exist) is maintained through theater. The Mountain perspective (analytical naturalization) triggers false-summit detection: the engine will identify beneficiaries and conclude that 'someone must be supreme' is naturalization of a choice that benefits parliament. The mandatrophy is resolved by observing that the constraint IS a Tangled Rope: the coordination function is real (parliament does enable governance through stable interpretive authority), and the extraction is real (majorities override minorities without recourse). Both are structural, not one hidden and one revealed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_mandate_sufficiency,
    'Does election victory constitute sufficient legitimacy for absolute interpretive authority, or does it require constraint by entrenched individual rights?',
    'Historical analysis of majoritarian overreach in parliamentary supremacy systems vs. judicial constraint systems; documentation of rights violations occurring post-election when unchecked; comparative institutional performance data.',
    'If electoral mandate sufficient: parliamentary supremacy classification holds (Rope from legislature perspective). If mandate requires rights constraint: shifts to coordinate construction reading (Tangled Rope even from legislature perspective, with judiciary as coordinate authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_mandate_sufficiency, preference, 'Whether electoral mandate justifies unconstrained interpretive authority').

omega_variable(
    reading_foreclosure_status,
    'Does parliamentary supremacy logically foreclose judicial supremacy within the same constitutional framework, or can both readings coexist as live positions?',
    'Logical analysis: can a framework simultaneously grant parliament ultimate interpretive authority AND grant courts authority to void parliamentary acts? Examination of historical systems where both claims were asserted (UK pre-2004, post-Human Rights Act 1998) to identify whether coexistence is structurally or merely politically unstable.',
    'If logically foreclosing: change reading_relations to ''forecloses'' for both siblings. If coexisting: maintain ''coexists_with''. This determines whether the readings are incompatible premises or live competitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether parliamentary supremacy logically forecloses judicial supremacy').

omega_variable(
    natural_law_vs_constructed_choice,
    'Is parliamentary supremacy a natural law (inevitable structure of democratic legitimacy), or a constructed institutional choice that benefits identifiable actors?',
    'Comparative constitutional analysis: historical emergence of parliamentary supremacy in specific jurisdictions (UK, Commonwealth); contrast with judicial supremacy emergence in others (USA, post-1945 Germany); identification of actor coalitions benefiting from each choice; documentation of deliberate institutional selection rather than discovered imperative.',
    'If natural law: analytical observer mountain classification confirmed. If constructed: false summit detected; reclassify based on beneficiary extraction (likely Tangled Rope). The gap reveals whether ''logical necessity'' frames a contingent institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, empirical, 'Whether parliamentary supremacy is natural law or constructed institutional choice').

omega_variable(
    committer_foreclosure_question,
    'This constraint instantiates the parliamentary supremacy reading of constitutional interpretive authority. If the judicial supremacy reading were to establish that courts have ultimate authority over constitutional meaning, would this logically foreclose the parliamentary supremacy claim within the same legal framework, or would they coexist as contestable positions?',
    'Doctrinal analysis within jurisprudence: examine whether ''parliament is supreme'' and ''courts are supreme'' are formally incompatible (foreclosure) or whether they describe different institutional settlements that competing factions claim (coexistence). Test case: UK Human Rights Act 1998, which introduced ''declarations of incompatibility'' — courts identify conflicts without striking down legislation. Does this coexist with parliamentary supremacy or undermine it?',
    'Shapes reading_relations structure: if foreclosing, the sibling reading cannot be held in the same framework (rare). If coexisting, both readings remain live political commitments held by different actors (more typical). If influences, then this reading creates pressures that reshape the sibling''s conditions without resolving the dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_foreclosure_question, conceptual, 'Reading foreclosure: does parliamentary supremacy logically rule out judicial supremacy?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsupr_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(parsupr_tr_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(parsupr_tr_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(parsupr_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(parsupr_be_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(parsupr_be_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(parsupr_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(parsupr_su_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement(parsupr_su_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, majoritarian_override_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_rights_protection_constraint).

% DUAL FORMULATION NOTE:
% The constitutional_interpretive_authority kernel decomposes into three constraint stories (three readings), each with distinct ε values and beneficiary/victim structures. Parliamentary supremacy reading (this story): ε=0.52, Tangled Rope. Judicial supremacy reading (sibling): ε≈0.48 (moderate extraction through judicial overreach; different beneficiary/victim structure). Coordinate construction reading (sibling): ε≈0.35 (lower extraction through institutional balance; both parliament and courts benefit; minorities less victimized). These are not the same constraint viewed three ways — they have genuinely different ε values, different enforcement mechanisms, and different victim sets. The readings coexist because jurisdictions and jurisprudential traditions hold different commitments, not because the constraint has a measurement-dependent observable. Each reading is ε-invariant within its frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
