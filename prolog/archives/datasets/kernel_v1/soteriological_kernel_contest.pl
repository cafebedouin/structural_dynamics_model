% ============================================================================
% CONSTRAINT STORY: soteriological_kernel_contest
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soteriological_kernel_contest, []).

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
 *   constraint_id: soteriological_kernel_contest
 *   human_readable: The Soteriological Authority Contest: One Kernel or Multiple?
 *   domain: historical_epistemology/religious_commitment_systems
 *
 * SUMMARY:
 *   The Protestant Reformation instantiates a structured authority contest
 *   over the nature of salvation (soteriological doctrine), the grounding of
 *   that doctrine (scripture vs. tradition vs. reason), and the institutional
 *   mediation of salvific knowledge (priesthood, sacraments, grace
 *   mechanism). The constraint emerges not as a single doctrinal dispute but
 *   as a cascade of coupled institutional challenges: to Roman authority
 *   structure, to scholastic epistemology, to clerical monopoly on biblical
 *   interpretation, and to the sacramental mediation of grace. The NON-BREAK
 *   library's prediction that composite structures should be absorbed as
 *   compositeness (rather than forced into singular kernel reduction)
 *   suggests that the Reformation may represent the convergence of multiple
 *   distinct kernels suddenly perceived as a single contest. The key
 *   analytical question is not 'who is right about salvation?' but 'are we
 *   analyzing one contested kernel with multiple readings (Luther vs. Rome
 *   vs. Zwingli as readings of salvation itself) or multiple kernels that
 *   historical narrative has unified retroactively?' This story examines the
 *   constraint from the perspective of commitment-system theory: analyzing
 *   what kernels are actually contested and whether they are fundamentally
 *   coupled or merely historically entangled.
 *
 * KEY AGENTS:
 *   - Roman Ecclesiastical Authority: Primary beneficiary (institutional/arbitrage) — maintains extraction through doctrinal monopoly; Counter-Reformation is performative theater (Piton classification)
 *   - Reformed Ecclesiastical Authority: Secondary beneficiary (institutional/arbitrage) — offers coordination solution (sola scriptura) while extracting through alternative authority structure
 *   - Princely Territorial Power: Tertiary beneficiary (powerful/mobile) — arbitrates doctrinal conflict to legitimize political consolidation; mobility allows switching allegiance
 *   - Scholastic Theological Authority: Primary victim (institutional/constrained) — displaced by both reformed doctrine and princely appropriation; loses epistemic autonomy
 *   - Parish Priests: Secondary victim (powerless/trapped) — caught between competing authority claims; cannot exit without losing livelihood
 *   - Rural Peasant Communities: Tertiary victim (moderate/constrained) — experience radical uncertainty about salvific truth; suppression of heterodox alternatives (Anabaptism, folk piety)
 *   - Analytical Observer: Perspective (analytical/analytical) — risks naturalizing institutional conflict as theological indeterminacy (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soteriological_kernel_contest, 0.58).
domain_priors:suppression_score(soteriological_kernel_contest, 0.68).
domain_priors:theater_ratio(soteriological_kernel_contest, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soteriological_kernel_contest, extractiveness, 0.58).
narrative_ontology:constraint_metric(soteriological_kernel_contest, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(soteriological_kernel_contest, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soteriological_kernel_contest, snare).
narrative_ontology:human_readable(soteriological_kernel_contest, "The Soteriological Authority Contest: One Kernel or Multiple?").
narrative_ontology:topic_domain(soteriological_kernel_contest, "historical_epistemology/religious_commitment_systems").

domain_priors:requires_active_enforcement(soteriological_kernel_contest).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(soteriological_kernel_contest, 'e9186b1f-f83e-40c1-a6f3-c2b5028680f1').
narrative_ontology:cs_kernel_codification('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', distributed).
narrative_ontology:cs_authority_grounding('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', extraction).
narrative_ontology:cs_interpretation_layer_present('e9186b1f-f83e-40c1-a6f3-c2b5028680f1').
narrative_ontology:cs_reading_relation('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', lutheran_salvific_mechanism, forecloses).
narrative_ontology:cs_reading_relation('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', calvinist_election_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', anabaptist_community_salvation, coexists_with).
narrative_ontology:cs_reading_relation('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', scholastic_cooperative_grace, forecloses).
narrative_ontology:cs_axiom('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', foundational, scriptural_sufficiency_salvific_doctrine).
narrative_ontology:cs_axiom_status(scriptural_sufficiency_salvific_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', scriptural_sufficiency_salvific_doctrine, deontological).
narrative_ontology:cs_axiom('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', foundational, papal_institutional_authority_necessity).
narrative_ontology:cs_axiom_status(papal_institutional_authority_necessity, overridden).
narrative_ontology:cs_axiom_grounding('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', papal_institutional_authority_necessity, conventional).
narrative_ontology:cs_axiom('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', foundational, grace_imputation_vs_cooperation_mechanism).
narrative_ontology:cs_axiom_status(grace_imputation_vs_cooperation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', grace_imputation_vs_cooperation_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', roman_sacramental_hierarchy).
narrative_ontology:cs_drift_state('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', reformation_eruption_1517_1530, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('e9186b1f-f83e-40c1-a6f3-c2b5028680f1', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soteriological_kernel_contest, reformed_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(soteriological_kernel_contest, princely_political_power).
narrative_ontology:constraint_victim(soteriological_kernel_contest, scholastic_theological_authority).
narrative_ontology:constraint_victim(soteriological_kernel_contest, lower_clergy_epistemic_autonomy).
narrative_ontology:constraint_victim(soteriological_kernel_contest, peasant_religious_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARISH PRIEST (SNARE) — Caught between Rome's doctrinal enforcement and emerging reformed authority claims. Cannot exit either system without losing livelihood, status, and community standing. Bears maximum extraction as the institutional conflict is weaponized through local churches. No meaningful exit option; trapped in contradictory mandates.
constraint_indexing:constraint_classification(soteriological_kernel_contest, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL PEASANT COMMUNITY (SNARE) — Experiences radical uncertainty about salvific truth claims. Both old and new authorities claim exclusive access to salvation; conflicting doctrine generates existential anxiety. Exit consists of suppressed heterodox alternatives (Anabaptism, folk practice). High suppression; genuine constrained mobility but at severe cost.
constraint_indexing:constraint_classification(soteriological_kernel_contest, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRINCELY TERRITORIAL AUTHORITY (TANGLED ROPE) — Benefits from the contested soteriological kernel as a tool for state consolidation. The conflict between Rome and reformed claims enables territorial princes to arbitrate doctrine, thereby legitimizing their own authority. Genuine coordination function (resolving doctrinal chaos) mixed with extraction (expanding political power through religious arbitration). Mobile exit option via either allegiance choice, but benefits from maintaining the tension.
constraint_indexing:constraint_classification(soteriological_kernel_contest, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMED ECCLESIASTICAL AUTHORITY (ROPE) — Experiences the contest as a coordination mechanism: the soteriological dispute requires clear doctrine and organized teaching. The reformation offers to solve doctrinal chaos through systematic theology and sola scriptura. Effective extraction runs from the old church to the new; arbitrage is available to institutional authority (switching allegiance from Rome to reformed polity). Net beneficiary.
constraint_indexing:constraint_classification(soteriological_kernel_contest, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ROMAN ECCLESIASTICAL AUTHORITY (PITON) — Maintains doctrinal authority through institutional inertia and suppression mechanisms rather than functional legitimacy. The Counter-Reformation represents theater: elaborate doctrinal restatement without fundamental revision of contested kernel. Sees its own authority as degraded but continues ritual enforcement. Theater ratio reflects the growing gap between institutional claims and actual persuasive power.
constraint_indexing:constraint_classification(soteriological_kernel_contest, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, soteriological questions about the nature of salvation and grace have an inherent indeterminacy rooted in the limits of human theological reasoning. No amount of institutional authority can 'solve' whether salvation is predestined, earned, or given — these are constitutive indeterminacies in the framework itself. This perspective sees the contest as an immutable feature of Christian theology: an irreducible problem-space that generates recurrent structural conflict. However, the beneficiary and victim data reveal this as a false summit — the naturalization of institutional conflict serves identifiable extractive interests.
constraint_indexing:constraint_classification(soteriological_kernel_contest, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soteriological_kernel_contest_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soteriological_kernel_contest, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soteriological_kernel_contest, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soteriological_kernel_contest, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soteriological_kernel_contest, TR),
    TR >= 0.70.

:- end_tests(soteriological_kernel_contest_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Reformation generates extractive asymmetries across multiple margins simultaneously: beneficiary authorities gain expanded influence, clerical mediators lose autonomy, peasant communities face suppressed alternatives. The extractiveness value reflects that the constraint is not pure extraction (χ ≥ 0.66 requires suppression ≥ 0.60 and ε ≥ 0.46) — there is genuine coordination being solved (doctrinal clarity, scriptural access, institutional rationalization) — but the coordination is asymmetrically distributed. Reformed authorities capture the coordination benefit while older authorities bear displacement costs and peasants bear uncertainty costs. The 0.58 value reflects this hybrid structure. Suppression (0.68): Moderate-high and rising. The suppression trajectory from 1450 (0.50) to 1530 (0.68) tracks the escalation of enforcement mechanisms: from doctrinal debate to institutional anathemas to military campaigns. Heterodox alternatives (Anabaptism, mystical traditions, folk piety) are increasingly suppressed. The barrier to exit from either authority system rises sharply as the contest becomes weaponized. Theater ratio (0.81): High and rising. The Counter-Reformation generates elaborate ritual restatement without fundamental doctrinal revision (e.g., Tridentine affirmation of free will alongside predestination despite the contradiction). Parish priests are increasingly required to perform doctrinal correctness through ritual conformity rather than through substantive theological engagement. The growth in theater from 1450 (0.55) to 1530 (0.81) reflects institutional hardening and ritual intensification as the functional coordination mechanisms degrade.
 *
 * PERSPECTIVAL GAP:
 *   This constraint resolves the perspectival gap by showing that each institutional position perceives a fundamentally different constraint. The beneficiary sees coordination; the victim sees extraction; the displaced authority sees degradation; the rising authority sees liberation; the prince sees opportunity; the observer risks seeing inevitability. These are not different views of the same constraint — they are genuinely different structural positions generating different constraint experiences. The mandatrophy is resolved by recognizing that the 'what type is this?' question has no single answer: it is Snare from the victim perspective, Rope from the beneficiary perspective, and Tangled Rope at the zone where both coordination and extraction are genuine. The analytical observer's mountain is a false summit: the 'indeterminacy' naturalizes what is actually institutional conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each perspective's structural relationship to the extraction flow. Beneficiary authorities (reformed, princely) have low d (around 0.15–0.25) because the extraction runs toward them; their f(d) is near zero, yielding negative effective extraction (they perceive coordination, not extraction). Victims (peasants, parish priests) have high d (around 0.85–0.95) because the extraction runs from them; their f(d) is around 1.15–1.42, amplifying experienced extraction. The Roman authority has intermediate d (around 0.60–0.70) because it is being displaced — it retains some institutional power (arbitrage option) but is losing its extraction source. The peasant community's trapped exit option (no exit from soteriological uncertainty without genuine heterodox community building) yields the highest d. The prince's mobile exit option (can choose allegiance) yields the lowest institutional d despite being a beneficiary, because mobility reduces total experienced extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_unity_vs_multiplicity,
    'Is the Reformation a contest over ONE contested soteriological kernel, or does it represent the convergence of multiple structurally distinct kernels (grace mechanism, authority structure, epistemic access, priestly mediation) that the historical narrative unifies retroactively?',
    'Genealogical decomposition: tracing each doctrinal axis (grace theology, authority grounding, scriptural access, sacramental mechanism) to its pre-Reformation origins and identifying whether these axes were always cosmically linked or became coupled only during the 16th century institutional crisis. NON-BREAK library predicts compositeness; map structural dependencies to verify.',
    'If single kernel: the Reformation is fundamentally ONE contested claim (e.g., ''what is the mechanism of salvation?'') with multiple readings (Aquinas vs. Luther vs. Zwingli). Classification remains Snare from victim perspectives, Rope/Tangled from authority perspectives. If multiple kernels: the Reformation is overdetermined convergence — each kernel has its own reading structure, and the apparent unity is narrative artifact. This would suggest decomposing into separate constraint stories per kernel, with network linkage showing how conflicting authorities attempted to couple them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_unity_vs_multiplicity, conceptual, 'Whether the soteriological dispute is one contested kernel or multiple coupled kernels').

omega_variable(
    authority_grounding_shift_mechanism,
    'Does the Reformation represent a shift in the SOURCE of theological authority (from institutional hierarchy to scripture/reason) or a dispute over the CONTENT of theology while authority grounding remains institutional?',
    'Epistemic genealogy: examine whether Luther''s sola scriptura represents a genuine alternative authority structure or whether it functionalizes as Luther''s personal authority delegitimizing Rome''s. Compare scriptural exegesis variance under sola scriptura to exegetical variance under papal authority — if variance increases, the authority shift is structural; if variance remains bounded by reformed institutional pressure, the shift is rhetorical.',
    'If genuine shift: multiple authority structures are live simultaneously (distributed authority system). This changes the kernel classification to ''distributed'' codification. The Reformation becomes a permanent structural pluralism rather than a solvable contest. If rhetorical: the reformed churches replicate Roman hierarchicalism under different labels, and the extraction mechanism is obscured by false claims to liberated epistemic access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_shift_mechanism, empirical, 'Whether authority shift in the Reformation is structural or rhetorical').

omega_variable(
    soteriological_indeterminacy_scope,
    'Which aspects of the salvation doctrine are genuinely indeterminate (rooted in limits of reasoning) versus socially constructed (rooted in institutional power claims)? Specifically: Is predestination an indeterminate theological problem, or a constructed doctrine serving institutional control narratives?',
    'Counterfactual institutional analysis: If Rome had admitted uncertainty on predestination rather than systematizing Pelagianism, would the Protestant challenge have lost force? If reformed churches had acknowledged scriptural underdetermination rather than claiming sola scriptura clarity, would the sectarian fragmentation have been attenuated? Historical simulation via logical dependency mapping.',
    'If indeterminacy is genuine: theological conflict is inherent (mountain perspective is justified, but remains a false summit because beneficiaries exploit the indeterminacy). If socially constructed: the entire apparatus is extraction theater masquerading as doctrinal necessity. Classification remains Snare but the mandatrophy analysis shifts: the false summit is not ''theological indeterminacy is natural'' but ''institutional conflict is theologically necessary.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soteriological_indeterminacy_scope, conceptual, 'Scope of genuine vs. constructed indeterminacy in soteriological doctrine').

omega_variable(
    reformation_contingency_on_printing,
    'Is the Reformation structurally dependent on printing-press technology as a coordination mechanism, or would the soteriological contest have erupted identically without printing?',
    'Technology-counterfactual: Model whether MS copy networks could have achieved the distribution required for Luther''s doctrinal challenge. Examine whether pre-printing religious challenges (Waldensians, Lollards, Hussites) failed due to coordination limits or due to different structural conditions. If printing is necessary: the constraint includes technological infrastructure as a boundary condition.',
    'If structurally dependent on printing: the constraint is more narrowly temporal — the exact form of the Reformation (rapid ideological diffusion, vernacular reach, authority-decentralization) is bound to print technology. Classification and extraction mechanisms would be specific to print-era authority structures. If independent: the soteriological contest is civilization-scale and civilization-recurrent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_contingency_on_printing, empirical, 'Technological contingency of the Reformation''s scope and speed').

omega_variable(
    counter_reformation_sincerity,
    'Did the Counter-Reformation represent genuine theological response to Protestant challenges (i.e., substantive engagement with contested doctrine) or institutional theater designed to reassert authority without doctrinal concession?',
    'Textual analysis of Tridentine theology vs. pre-Tridentine scholasticism: identify which doctrinal positions shifted, which remained constant despite Protestant critique. Compare Tridentine reasoning on grace, predestination, and scriptural authority to Luther/Calvin positions — calculate overlap and conceptual distance. High overlap with institutional restatement = theater; genuine convergence with reformed positions = substantive response.',
    'If theater: the Piton classification is confirmed — the Reformation is not settled by Counter-Reformation because the settlement is performative rather than substantive. The constraint persists. If substantive: the constraint may have genuinely resolved toward a new equilibrium (though historical data suggests it did not). Current evidence: Tridentine anathemas on sola scriptura and predestination suggest theater rather than convergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_reformation_sincerity, empirical, 'Whether Counter-Reformation was substantive theological response or institutional theater').

omega_variable(
    false_summit_natural_theology_frame,
    'Is the mountain perspective''s claim (''soteriological indeterminacy is inherent to theology'') itself a beneficiary cover story masking institutional extraction?',
    'Investigate whether the naturalizing language (''incompatible free will and predestination,'' ''unknowable divine will'') appears in beneficiary authority structures as justification for institutional mediation. If clerics invoke indeterminacy to justify their necessity as interpreters, the natural-law frame is instrumentalized. If the indeterminacy is invoked to license pluralism instead, it may be genuine.',
    'If instrumentalized: the mountain classification fails FSM certification. The constraint is a snare disguised as natural law. The beneficiary (institutional authority) maintains itself through claims that the problem is unsolvable rather than through claims that they solve it. If genuine: the mountain stands, though victims experience it as institutional extraction. This is the critical mandatrophy test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_theology_frame, conceptual, 'False summit detection: Is theological indeterminacy natural or instrumentalized?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soteriological_kernel_contest, 1450, 1530).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soter_tr_t1450, soteriological_kernel_contest, theater_ratio, 1450, 0.55).
narrative_ontology:measurement(soter_tr_t1500, soteriological_kernel_contest, theater_ratio, 1500, 0.72).
narrative_ontology:measurement(soter_tr_t1530, soteriological_kernel_contest, theater_ratio, 1530, 0.81).

% Extraction over time
narrative_ontology:measurement(soter_be_t1450, soteriological_kernel_contest, base_extractiveness, 1450, 0.42).
narrative_ontology:measurement(soter_be_t1500, soteriological_kernel_contest, base_extractiveness, 1500, 0.51).
narrative_ontology:measurement(soter_be_t1530, soteriological_kernel_contest, base_extractiveness, 1530, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soter_su_t1450, soteriological_kernel_contest, suppression_requirement, 1450, 0.5).
narrative_ontology:measurement(soter_su_t1500, soteriological_kernel_contest, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(soter_su_t1530, soteriological_kernel_contest, suppression_requirement, 1530, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soteriological_kernel_contest, identity_coordination).
narrative_ontology:affects_constraint(soteriological_kernel_contest, printing_press_authority_distribution).
narrative_ontology:affects_constraint(soteriological_kernel_contest, peasant_reformation_heterodoxy).
narrative_ontology:affects_constraint(soteriological_kernel_contest, jesuit_counter_reformation_theater).

% DUAL FORMULATION NOTE:
% The soteriological kernel contest represents a single historical event with multiple structural decompositions. An alternative analysis would separate into constraint families: (1) salvation-mechanism dispute (theological kernel), (2) authority-structure shift (institutional kernel), (3) priesthood-necessity contest (functional kernel), (4) scripture-authority dispute (epistemic kernel). The current story maintains unity following NON-BREAK absorption principle; decomposition should link via affects_constraints showing how kernels are mutually reinforcing during the Reformation period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soteriological_kernel_contest, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
