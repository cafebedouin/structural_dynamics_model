% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   Qualified immunity doctrine establishes a legal shield that protects
 *   officers from personal civil liability for constitutional violations
 *   unless the violated right was 'clearly established' at the time of
 *   conduct. This constraint story instantiates ONE reading of the contested
 *   immunity kernel: the protective scaffold reading, which frames immunity
 *   as a necessary institutional protection enabling vigorous law enforcement
 *   without paralyzing fear of bad-faith litigation. This reading emphasizes
 *   the coordination benefit to officers and agencies (stable framework for
 *   enforcement conduct, protection against frivolous suits) alongside the
 *   asymmetric cost to victims (systematic remedy denial). The protective
 *   scaffold reading coexists with two sibling readings: (1) the
 *   accountability void reading, which frames immunity as a mechanism that
 *   systematically forecloses remedies for constitutional violations and
 *   creates perverse incentives for misconduct; and (2) the constitutional
 *   fidelity reading, which frames immunity as a doctrinal illegitimacy that
 *   violates the Fourteenth Amendment's promise of equal protection and due
 *   process. This story generates the protective scaffold reading as a
 *   coherent, ε-invariant constraint without hedging across readings. The
 *   structural data—moderate base extractiveness (0.48), significant
 *   suppression (0.52), and rising theater ratio (0.42→0.58)—reflect the
 *   doctrine's dual nature: genuine protection for officers against frivolous
 *   litigation combines with systematic denial of remedy for violation
 *   survivors, mediated through judicial discretion in 'clearly established
 *   law' determinations. The rising theater ratio (0.42 to 0.58 over 40
 *   years) tracks increasing doctrinal complexity and procedural theater:
 *   courts apply qualified immunity at the summary judgment stage, often
 *   without full factual development, creating a procedurally efficient but
 *   legally opaque remedy denial mechanism.
 *
 * KEY AGENTS:
 *   - Constitutional Violation Survivors: Primary victims (powerless/trapped) — no meaningful remedy under qualified immunity; bear full cost of officer misconduct
 *   - Police Officers: Primary beneficiaries (institutional/arbitrage) — protected from personal liability; can enforce vigorously without personal litigation risk
 *   - Law Enforcement Agencies: Secondary beneficiary (institutional/arbitrage) — avoid vicarious liability for officer misconduct absent gross negligence; benefit from officer protection
 *   - Civil Rights Coalition: Organized agent (organized/mobile) — can litigate novel claims and pursue legislative reform; structurally constrained by 'clearly established law' gate
 *   - Federal Judiciary: Powerful institutional actor (powerful/mobile) — coordinates legal doctrine through precedent; wields discretion through 'clearly established' interpretation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable enforcement necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.52).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'fbb49067-c73e-4a7a-bb7b-9b016431c413').
narrative_ontology:cs_kernel_codification('fbb49067-c73e-4a7a-bb7b-9b016431c413', fixed_text).
narrative_ontology:cs_authority_grounding('fbb49067-c73e-4a7a-bb7b-9b016431c413', lineage).
narrative_ontology:cs_interpretation_layer_present('fbb49067-c73e-4a7a-bb7b-9b016431c413').
narrative_ontology:cs_reading_relation('fbb49067-c73e-4a7a-bb7b-9b016431c413', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbb49067-c73e-4a7a-bb7b-9b016431c413', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('fbb49067-c73e-4a7a-bb7b-9b016431c413', foundational, officer_protection_necessary_for_vigorous_enforcement).
narrative_ontology:cs_axiom_status(officer_protection_necessary_for_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('fbb49067-c73e-4a7a-bb7b-9b016431c413', officer_protection_necessary_for_vigorous_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('fbb49067-c73e-4a7a-bb7b-9b016431c413', foundational, qualified_immunity_proportionate_cost_distribution).
narrative_ontology:cs_axiom_status(qualified_immunity_proportionate_cost_distribution, holdable).
narrative_ontology:cs_axiom_grounding('fbb49067-c73e-4a7a-bb7b-9b016431c413', qualified_immunity_proportionate_cost_distribution, deontological).
narrative_ontology:cs_reference_frame('fbb49067-c73e-4a7a-bb7b-9b016431c413', harlow_protection_doctrine).
narrative_ontology:cs_drift_state('fbb49067-c73e-4a7a-bb7b-9b016431c413', contemporary_2020_2026, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('fbb49067-c73e-4a7a-bb7b-9b016431c413', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL VIOLATION SURVIVOR (SNARE) — No meaningful exit from immunity shield. Victim bears full cost of officer misconduct with no remedy; structural barriers to litigation (qualified immunity plus summary judgment) create total suppression. Experiences maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LAW ENFORCEMENT AGENCY (ROPE) — Benefits from immunity shield: officers can pursue enforcement without paralyzing fear of personal litigation; agencies avoid vicarious liability for officer misconduct absent gross negligence. Experiences the constraint as pure coordination benefit with negligible extraction cost. Net beneficiary.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL POLICE OFFICER (TANGLED ROPE) — Protected from frivolous litigation (genuine coordination benefit) but constrained by qualified immunity's 'clearly established law' standard, which creates uncertainty and career risk of adverse judgments despite immunity. Mixed: genuine protection against bad-faith suits plus extraction via liability exposure on edge cases. Exit options constrained by career dependence on law enforcement institutions.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized agents see both coordination and extraction. The doctrine creates a stable (if contested) framework for officer conduct norms and litigation procedures (coordination), but also systematically channels remedies away from victims and toward immunity (asymmetric extraction from violation survivors). Coalition has agency and exit (can litigate novel claims or pursue legislative reform) but structural constraints from judicial interpretation of 'clearly established' doctrine.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY/FEDERAL APPEALS COURTS (TANGLED ROPE) — Powerful institutional actors who coordinate legal doctrine through precedent (genuine coordination function) while extracting discretion through the 'clearly established law' gate, which allows fact-intensive judgment calls about whether law was clearly established at the time of the conduct. Mobility: courts can narrow or expand immunity through doctrine.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this reading frames immunity as a natural necessity: vigorous law enforcement requires some shield from frivolous litigation; without it, officers become risk-averse and enforcement capacity deteriorates. This perspective treats the immunity-protection tradeoff as an immutable feature of any functional enforcement system. FALSE SUMMIT CANDIDATE: the protective framing naturalizes what is actually a contingent institutional choice between immunity and other remedial/accountability mechanisms.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__protective_scaffold_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48): Moderate-high. The protective scaffold reading acknowledges genuine protection for officers (coordination benefit reducing frivolous litigation exposure) but quantifies asymmetric cost to victims (systematic remedy denial). The moderate value reflects that some victims can litigate successfully (where law was clearly established) and some suits are indeed frivolous (validating the coordination function), but a significant cohort is foreclosed entirely. The value increased from 0.32 (1982–1987, early immunity doctrine) to 0.48 (2020–2026) as the doctrine entrenched and courts narrowed 'clearly established law' thresholds. Suppression (0.52): Moderate-high. Victims face structural barriers: (1) qualified immunity at summary judgment forecloses many suits before discovery; (2) 'clearly established law' gate requires prior case with identical facts (very high standard); (3) officers have incentive to avoid documentation; (4) institutional resistance to discovery in police misconduct cases. But not total suppression—some victims prevail (where precedent is clear), creating 25-30% success rate for claims that reach the merits (data varies by circuit). Theater ratio (0.58): Moderate-high. Qualified immunity determinations involve procedural theater (summary judgment briefing, 'clearly established law' analysis, qualified immunity panels) that appears to resolve the merits but often forecloses full factual development. The rising trajectory reflects increasing procedural complexity: courts now apply qualified immunity before discovery, creating efficiency theater—fast resolution that appears decisive but lacks the evidential grounding of full trial. This theater has increased as doctrinal sophistication deepened and Supreme Court decisions narrowed immunity scope.
 *
 * PERSPECTIVAL GAP:
 *   The protective scaffold reading creates maximal perspectival divergence between victims (snare, d=0.95) and beneficiaries (rope, d=0.05). This gap is not a failure of classification—it is the analytical signal revealing that the constraint operates through asymmetric extraction channeled via institutional asymmetry. The victim cannot organize litigation (powerless), cannot exit the jurisdiction (trapped), cannot change the legal rule (no structural power). The officer can decline risky enforcement (constrained exit), can appeal adverse judgments (institutional support), can transfer departments or retire (career options). The same doctrine is protective shield to one and trap to the other. This gap is why the protective scaffold reading must coexist with the accountability void reading—both are correct observations of the same structural phenomenon from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural relationship to the immunity constraint: (1) Constitutional violation survivors: d ≈ 0.95 (pure targets, no benefits, trapped exit). They are the identified victims in base_properties and experience maximum extraction (f(d) → 1.42 at powerless power level). (2) Police officers: d ≈ 0.10 (near-pure beneficiaries, constrained exit via career dependence creates moderate d rather than true arbitrage). They benefit from immunity protection but face uncertainty on 'clearly established law' margin. (3) Law enforcement agencies: d ≈ 0.05 (institutional beneficiaries with arbitrage exit—can lobby for doctrine expansion or voluntary accountability programs). (4) Civil rights coalition: d ≈ 0.60 (mixed: organized capacity to litigate novel claims increases d downward from victim threshold; capacity to pursue legislative reform provides exit options). (5) Federal judiciary: d ≈ 0.50 (symmetric: courts benefit from immunity (reduces docket, filters frivolous suits) and bear costs (public criticism, constitutional legitimacy erosion). The directionality pattern produces high extraction experienced by victims (snare perspective) and low extraction experienced by beneficiaries (rope perspective), creating the perspectival gap. No directionality overrides are necessary—the automatic derivation from beneficiary/victim declarations plus exit options correctly captures the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The protective scaffold reading resolves mandatrophy by disambiguating the kernel: is immunity a protective necessity (scaffold/rope) or a remedy-denial extraction mechanism (snare)? The answer is BOTH, perspectivally. The protective scaffold reading holds this tension: genuine protection + genuine extraction, unified as tangled_rope in the base classification. The analytical natural law observer who sees immunity as an immutable feature of any enforcement system is engaging in false summit naturalization—treating a contingent 1982 doctrine with identifiable beneficiaries (officers, agencies) as a law of nature. The mandatrophy resolves by showing that all six types are legitimate readings of the immunity doctrine from different observational positions: (1) mountain (natural law necessity—false summit candidate); (2) rope (officer protection—legitimate but beneficiary-centric); (3) tangled rope (mixed protection and extraction—central reading); (4) snare (victim extraction—legitimate from victim position); (5) scaffold (temporary measure with sunset—only if doctrine is understood as transitional); (6) piton (degraded accountability theater—if immunity persists despite doctrinal erosion). The protective scaffold reading occupies the tangled rope center, acknowledging both coordination (officer protection, frivolous suit reduction) and extraction (remedy denial, asymmetric outcome distribution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_clarity_threshold,
    'What degree of precedent specificity constitutes ''clearly established law'' sufficient to defeat qualified immunity? Is the threshold calibrated to protect legitimate officer conduct or to shield misconduct?',
    'Empirical analysis of Supreme Court qualified immunity decisions (2000–present): correlation between threshold stringency and outcome reversal rates; comparison of qualified immunity success rates across circuit courts with different ''clearly established'' interpretations; analysis of whether officers prevail despite factual similarity to prior precedent.',
    'If threshold is stringent (requiring identical factual circumstances): immunity protects against frivolous suits but creates systematic remedy denial (snare for victims). If threshold is permissive (requiring only general principle): immunity provides meaningful protection without categorical exclusion (true tangled rope). Classification hinges on empirical calibration of the ''clearly established'' standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearly_established_law_clarity_threshold, empirical, 'Calibration of clearly established law threshold in qualified immunity doctrine').

omega_variable(
    alternative_accountability_mechanisms_sufficiency,
    'Do alternative accountability mechanisms (department discipline, criminal prosecution, civil injunction, civil rights monitoring) provide adequate remedy and deterrence absent qualified immunity?',
    'Comparative analysis: jurisdictions with limited qualified immunity (state law, international) vs federal baseline; measurement of misconduct rates, victim remedies, and officer conduct changes; interview data from departments operating under different accountability regimes.',
    'If alternatives are sufficient: qualified immunity is extraction mechanism (snare from victim perspective), not protective necessity (snare becomes dominant classification). If alternatives inadequate: immunity provides genuine coordination benefit (scaffold or rope remains defensible). This omega directly addresses the sibling-reading contrast between protective scaffold and accountability void readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accountability_mechanisms_sufficiency, empirical, 'Whether alternative accountability mechanisms can replace qualified immunity').

omega_variable(
    officer_behavior_risk_aversion_calibration,
    'What degree of personal litigation exposure actually produces risk-aversion that impairs legitimate law enforcement? Is the protective scaffold targeting a real behavioral threshold or a speculative one?',
    'Comparative study of officer decision-making under qualified immunity vs liability exposure (using state law variation as natural experiment); measurement of enforcement activity, escalation rates, and outcome quality under different legal regimes; simulation modeling of officer utility functions under varying litigation cost assumptions.',
    'If officers show significant behavior change above threshold: scaffold framing is empirically grounded (protection is real and necessary). If behavior change is marginal: scaffolding is over-engineered relative to actual risk, and extraction dominates (snare becomes more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_behavior_risk_aversion_calibration, empirical, 'Behavioral threshold of litigation exposure causing risk-aversion in law enforcement').

omega_variable(
    reading_contest_scope,
    'Is this protective scaffold reading a coherent alternative to the accountability void reading within a single committed framework, or are the readings rooted in incompatible foundational premises?',
    'Doctrinal analysis: can a framework simultaneously hold ''officers need immunity protection'' (protective scaffold) and ''systematic remedy denial is unconstitutional'' (accountability void)? Examine how courts adjudicate between the readings; identify which axioms each reading holds as non-negotiable.',
    'If forecloses: the readings cannot coexist; one reading''s core premise directly contradicts the other''s. If coexists_with: both readings remain live despite disagreement (different coalitions hold each). This omega structures the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_scope, conceptual, 'Whether protective scaffold and accountability void readings logically foreclose one another').

omega_variable(
    natural_law_false_summit_ambiguity,
    'Is the immunity-protection tradeoff an immutable feature of any functional enforcement system (mountain), or a contingent institutional choice that benefits specific actors (snare or tangled rope)?',
    'Historical and comparative analysis: enforcement systems without qualified immunity (UK, Canada, international contexts); measurement of enforcement effectiveness and officer risk-aversion under alternative doctrines; doctrinal genealogy of qualified immunity (1982 Harlow decision and its contingent policy rationale vs claims of natural necessity).',
    'If immutable: mountain classification is correct (natural law frame). If contingent: mountain is a false summit; the protective scaffold reading is itself a constructed constraint with identifiable beneficiaries and victims. This is the core dispute between protective scaffold and false-summit perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_ambiguity, conceptual, 'Whether immunity-protection tradeoff is immutable law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qimmun_scaffold_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(qimmun_scaffold_tr_t15, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(qimmun_scaffold_tr_t40, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(qimmun_scaffold_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(qimmun_scaffold_be_t15, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(qimmun_scaffold_be_t40, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigation_bottleneck).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, police_accountability_void).

% DUAL FORMULATION NOTE:
% Qualified immunity kernel decomposes into three structurally distinct constraint stories: protective_scaffold_reading (this file) frames immunity as protection; accountability_void_reading frames it as remedy foreclosure; constitutional_fidelity_reading frames it as doctrinal illegitimacy. Each story has distinct ε, distinct beneficiary/victim structure, and distinct measurements. All three share the same kernel (Harlow/§1983) but generate different classifications from different observational positions. Link all three files via network.affects_constraints. The ε values differ because the readings measure different extraction mechanisms: protection-based ε measures coordination benefit vs frivolous suit cost; accountability ε measures remedy denial + misconduct incentive externality; constitutional ε measures doctrinal inconsistency with equal protection. These are not the same constraint viewed three ways—they are three constraints generated from one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
