% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status_parmenidean_rejection, []).

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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Exclusion of Zero from the Number Domain
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   The Parmenidean reading holds that nothing cannot exist or be coherently
 *   spoken of, and therefore that zero — a sign for what is not — is
 *   ontologically incoherent as a number. Instantiated as a social
 *   arrangement, this excluded zero from the number domain for over two
 *   millennia in the Greek-Latin tradition: arithmetic operations on it were
 *   undefined, astronomers got placeholder-only access, merchants were locked
 *   into fraud-prone Roman numerals by statute, and algebra stalled for lack
 *   of an operable null symbol. The arrangement carried a genuine
 *   coordination function (keeping ontology and arithmetic mutually
 *   consistent under the unit-plurality definition of number) alongside
 *   sharply asymmetric costs borne by computational practitioners, and it was
 *   actively enforced — curriculum control, marginalization of atomists,
 *   statutory cipher bans such as Florence's 1299 decree — until positional
 *   notation's demonstrated utility overwhelmed it between Fibonacci and the
 *   seventeenth century. KEY AGENTS (by structural relationship):
 *   parmenidean_dialectical_tradition — primary agenda setter
 *   (institutional/identity_locked), fixes the terms of coherent speech about
 *   nothing; scholastic_university_authorities — enforcement arm
 *   (institutional/constrained), administers curriculum and statute;
 *   greek_geometric_mathematicians — beneficiary (powerful/constrained),
 *   proportion-theory edifice shielded from degenerate cases;
 *   abacist_calculation_guilds — concentrated pecuniary beneficiary
 *   (organized/constrained), fee monopoly protected by cipher bans;
 *   observational_astronomers — primary payer (powerful/constrained),
 *   placeholder-only access to zero; merchant_accountkeepers — payer
 *   (moderate/trapped), Roman-numeral ledgers and fraud exposure;
 *   emerging_algebraists — payer (moderate/constrained), equation theory
 *   stalls without an operable zero; indian_arabic_zero_tradition — excluded
 *   holder of the working alternative (powerful/trapped); analytical_observer
 *   — sees the full three-reading structure. The claim/metric gap is
 *   deliberate: the reading itself presents the exclusion as ontological
 *   necessity (mountain-shaped from inside), while the authored metrics
 *   describe enforced coordination with asymmetric extraction — the engine
 *   measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.72).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.75).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.72).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Exclusion of Zero from the Number Domain").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, 'd0101f70-9f81-445c-8803-7e079b84397d').
narrative_ontology:cs_kernel_codification('d0101f70-9f81-445c-8803-7e079b84397d', fixed_text).
narrative_ontology:cs_authority_grounding('d0101f70-9f81-445c-8803-7e079b84397d', lineage).
narrative_ontology:cs_interpretation_layer_present('d0101f70-9f81-445c-8803-7e079b84397d').
narrative_ontology:cs_reading_relation('d0101f70-9f81-445c-8803-7e079b84397d', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('d0101f70-9f81-445c-8803-7e079b84397d', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('d0101f70-9f81-445c-8803-7e079b84397d', foundational, nonbeing_cannot_be_numbered).
narrative_ontology:cs_axiom_status(nonbeing_cannot_be_numbered, holdable).
narrative_ontology:cs_axiom_grounding('d0101f70-9f81-445c-8803-7e079b84397d', nonbeing_cannot_be_numbered, deontological).
narrative_ontology:cs_axiom('d0101f70-9f81-445c-8803-7e079b84397d', secondary, mathematics_science_of_being_alone).
narrative_ontology:cs_axiom_status(mathematics_science_of_being_alone, holdable).
narrative_ontology:cs_axiom_grounding('d0101f70-9f81-445c-8803-7e079b84397d', mathematics_science_of_being_alone, conventional).
narrative_ontology:cs_reference_frame('d0101f70-9f81-445c-8803-7e079b84397d', plenum_of_being_number_as_counted_plurality).
narrative_ontology:cs_drift_state('d0101f70-9f81-445c-8803-7e079b84397d', post_positional_adoption_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d0101f70-9f81-445c-8803-7e079b84397d', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, parmenidean_dialectical_tradition).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, scholastic_university_authorities).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, greek_geometric_mathematicians).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, abacist_calculation_guilds).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, observational_astronomers).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, merchant_accountkeepers).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, emerging_algebraists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulates and transmits the arguments that settle what can coherently be said about nothing: to speak of nothing is already to make it something, therefore no sign for it may enter rigorous talk of quantity. The tradition's authority rests on these arguments presenting themselves as necessary rather than chosen; abandoning them would dissolve the school's founding identity, so exit is unavailable from inside. It sets the terms in which every other participant must phrase any proposal about zero.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, parmenidean_dialectical_tradition, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Controls the curriculum, licenses teachers, and decides which texts and computational methods may be taught in the faculties of arts and theology. The framework's centrality fills chairs and orders the syllabus, and collecting that standing gives these bodies a durable stake in its continuance. They can suppress rival methods through statute and examination requirements, though they answer to patrons and canon law, which bounds their freedom of maneuver.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, scholastic_university_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, scholastic_university_authorities, beneficiary).

% Works in a tradition where number means a counted multitude of units and proof runs through magnitudes and proportions. On those definitions a sign for 'no units' has no work to do, and the exclusion spares their proportion theory a class of degenerate cases for which they have no tools. Their accumulated investment in the existing edifice makes retooling unattractive even where the missing techniques exist abroad.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, greek_geometric_mathematicians, beneficiary,
    powerful, generational, constrained, regional).

% Sells calculation as a service: counting-board masters who multiply, divide, and convert currencies for merchants who cannot do it themselves. Written cipher arithmetic with a zero column would let any clerk do the same work on paper for the cost of pen and ink, so the guilds back statutes restricting Arabic numerals in account books and stigmatize ciphers as suspect. Their livelihood is bound to the older methods; leaving the trade is the only exit.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, abacist_calculation_guilds, beneficiary,
    organized, biographical, constrained, national).

% Produces planetary tables and calendar corrections requiring vast chains of multiplications and divisions. They may use a round symbol as a blank in sexagesimal fractions — a placeholder — but may not operate on it: no carrying through it, no multiplying by it, no results stated in it. Every computation therefore runs through laborious intermediate steps, and transcription errors compound across long tables. The technique that would roughly halve their labor exists and circulates in translated manuscripts, but treating it as arithmetic marks them as philosophically careless before the faculties.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, observational_astronomers, payer,
    powerful, generational, constrained, continental).

% Keeps ledgers in Roman numerals under guild rules that forbid Arabic figures in official books. Addition and audit are slow, and the numeral forms invite alteration fraud — a single stroke turns VI into VII or IX. Penalties for cipher bookkeeping fall on them personally, and their customers and tax officials require the sanctioned formats, so they cannot unilaterally switch without losing legal standing.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, merchant_accountkeepers, payer,
    moderate, biographical, trapped, regional).

% Pushes the symbolic treatment of equations from Diophantine word problems toward general rules. Without a sign that can hold a result of 'nothing' and still participate in operations, equation theory stalls at cases a zero column would unify; their manuscripts route around the gap with verbal dodges that grow more baroque each generation. The framework that blocks them is administered by faculties they do not sit in.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, emerging_algebraists, payer,
    moderate, generational, constrained, continental).

% Holds a complete, working zero arithmetic — Brahmagupta's rules for addition, subtraction, multiplication, and division involving zero, carried through positional notation and algorithmic procedure. Their results circulate in translation, but the conversation about what zero IS happens in institutions where they have no seat; their framework is dismissed before argument as foreign or impious. Nothing they possess lets them enter the room where the decision is made.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, indian_arabic_zero_tradition, excluded,
    powerful, generational, trapped, continental).

% Compares the three readings of zero's status across their full histories — which survived, which collapsed, what each cost and whom — with access to the adoption record, the ban statutes, and the manuscript trails. Holds no stake in any reading's victory.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, abacist_calculation_guilds).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single consistent framework binding ontology to arithmetic: defining number as a counted plurality of units keeps mathematics a science of what-is, shields Eudoxan proportion theory from null-degenerate cases it cannot handle, and gives the learned community a shared criterion of coherent speech about quantity.
% TRANSFER_FUNCTION: Moves computational labor, error risk, and calculation fees onto those who must quantify — merchants, astronomers, algebraists — toward the abacist guilds, and moves epistemic authority and curricular centrality toward the metaphysical schools; rival notation systems are moved out of legitimacy altogether.
% ABSENT_VOICES: The Indian and Arabic zero-tradition — holders of a working, rule-complete zero arithmetic (Brahmagupta, al-Khwarizmi) — is structurally outside the conversation, dismissed as barbarous before contact and restricted after it; atomist defenders of the void are similarly marginalized. They would testify that zero is operationalizable and that the exclusion is a choice, not a necessity.
% DISAPPEARANCE_RATIONALE: If the exclusion vanished overnight — if zero were admitted as a number with defined operations — positional notation would spread immediately, commercial arithmetic costs and ledger fraud would drop, algebra would acquire its operational core centuries early, the abacist fee monopoly would evaporate, and the metaphysical schools would lose a load-bearing pillar of their union of ontology and mathematics.
% FOUNDING_PROBLEM: Reconcile the practice of counting with the metaphysical impossibility of nothing: how can a rigorous science of quantity contain a sign for what is not, without letting non-being smuggle itself into being through arithmetic?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the adoption record itself: Fibonacci's Liber Abaci (1202), the Italian botteghe d'abaco commercial arithmetic, and the eventual universal practice of Brahmagupta's rules demonstrate the problem was soluble without ontological catastrophe; modern histories of mathematics (Katz, Kline, Ifrah, Menninger) attest that the founding problem dissolved once zero's operational consistency was demonstrated, and no contemporary mathematical source attests it as live. The metaphysical question of nothing persists in cosmology and philosophy of physics, but detached from any constraint on number.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__parmenidean_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__parmenidean_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base properties are authored at the constraint's characteristic peak operating point (roughly t=18 on the interval grid, the late-thirteenth to fourteenth-century enforcement maximum): extractiveness 0.72 because the gap between available technique (documented Brahmagupta rules in translation) and permitted practice was then widest and the paying class broad; suppression 0.75 as a raw structural property — curriculum control, professional stigma, and statutory bans — noting suppression is NOT scaled by power or scope, only extractiveness is; theater_ratio 0.52 because by the peak roughly half of maintenance activity was rhetorical repetition of necessity arguments while practice eroded underneath; accessibility_collapse 0.58 because the argument collapses alternatives thoroughly from inside the framework while the concrete alternative remained importable from outside; resistance 0.55 reflecting atomist dissent early, algorist and commercial pressure late, scattered until the endgame. The measurement series run on one shared time grid (t = 0,1,5,8,12,16,18,20,22, centuries since the Eleatic formulation) with every tracked metric authored at every point. The arc is rise-peak-collapse, not cyclical: extraction accumulates as the alternative becomes known, peaks at the enforcement maximum, and collapses as commercial adoption outruns enforcement capacity; theater_ratio rises monotonically throughout — the classic substitution of performed necessity for functioning argument — and suppression_requirement ratchets up to its statutory peak then decays with the enforcement apparatus itself.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the arrangement presents as necessity: within the framework there are no degrees of freedom and nothing to enforce, because no alternative is thinkable — a mountain-shaped experience. From the payer seats the same structure presents as imposed cost with a visible, documented alternative they are barred from using. From the excluded seat it presents as arbitrary dismissal — a verdict rendered without hearing the parties who hold the working counterexample. These are three experiences of one structure; the engine computes per-seat classifications from the power, exit, and role data rather than from any authored adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the dialectical tradition and university authorities are subsidized in authority and standing, the geometric mathematicians in conceptual security, the abacist guilds in fee income. Payers derive high directionality, amplified toward full-target by their exit profiles: merchants are trapped by statute and customer requirement, astronomers and algebraists constrained by institutional gatekeeping. The excluded zero-tradition is the instructive case — powerful in resources and technique, yet sitting near the full-target end because its exit is blocked by discursive legitimacy rather than by material means; power without a seat in the conversation buys no relief. The analytical observer derives no directional stake. Continental scope modestly amplifies effective extraction for the paying seats by raising verification difficulty across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling counting with the impossibility of nothing — was solvable, and was effectively solved, once Brahmagupta's rules demonstrated that zero could carry defined operations without ontological catastrophe; the European arrangement nonetheless persisted roughly three centuries past that demonstration on institutional inertia and guild interest, which is why mandatrophy_resolved is declared and why the R5 interview records status=dead against verdict=world_rearranges — the mismatch that flags zombie maintenance. The tangled_rope classification prevents mislabeling in both directions: accepting the reading's own mountain-presentation would launder the arrangement as natural law and render its payers invisible, while a pure-extraction reading would erase the genuine coordination function (ontology-arithmetic coherence under the unit-plurality definition) that kept the arrangement stable for two millennia. Keeping both faces legible lets the theater trajectory do the temporal work: the series shows function decaying into performance as the mandate dies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the parmenidean_rejection reading of kernel zero_mathematical_status: does zero''s exclusion follow from the nature of being, or is it one contestable assignment among siblings (number_reading, placeholder_reading) with different victim sets and different fates?',
    'Read the sibling stories'' structural data side by side; the disagreement localizes to whether ''number'' is defined by ontological pedigree (this reading), by operational closure under rules (number_reading), or by notational function (placeholder_reading).',
    'If the operational-closure criterion wins, this constraint''s victim set swells to every actor needing positional efficiency and the arrangement collapses — the historical outcome; if the ontological-pedigree criterion holds, the exclusion stands as conceptual hygiene rather than imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the zero-status kernel; sibling readings instantiate different constraints.').

omega_variable(
    ontological_necessity_vs_constructed_enforcement,
    'Is the exclusion of zero a genuine limit on coherent thought (a fact about being) or a constructed arrangement maintained by school authority, curriculum control, and statute?',
    'Enforcement-lapse test: track whether the exclusion survives wherever enforcement capacity lapses while alternatives remain known — the historical record (commercial adoption of ciphers wherever bans lapsed) is the natural experiment.',
    'If constructed, the arrangement belongs to the enforced-coordination family with identifiable payers; if necessary, the payers'' costs are the price of coherence and the beneficiary structure is illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_necessity_vs_constructed_enforcement, conceptual, 'Natural-law presentation versus enforced-construct reality of the exclusion.').

omega_variable(
    victim_set_error_and_fraud_boundary,
    'Does the victim set extend beyond denied positional efficiency to all bearers of elevated error and fraud costs under Roman-numeral accounting?',
    'Forensic accounting history: compare alteration-fraud incidence and audit costs in Roman-numeral versus positional ledgers, including the fraud rationale recorded for the 1299 Florentine ban.',
    'A wider victim set raises measured extraction and sharpens the asymmetry between paying practitioners and protected incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_error_and_fraud_boundary, empirical, 'Boundary of the paying class: efficiency losses only, or error and fraud burdens too.').

omega_variable(
    placeholder_loophole_damping,
    'Did placeholder-only access to a zero symbol (Ptolemaic sexagesimal practice) materially damp the costs borne by the astronomical seat relative to full exclusion?',
    'Compare computational labor and error profiles in placeholder-only tables against full positional computation in Sanskrit and Arabic sources performing equivalent reductions.',
    'If the loophole damped costs materially, the astronomical seat sits below full-target directionality; if not, it sits at full target and the placeholder concession was cosmetic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_loophole_damping, empirical, 'Whether partial notation access reduced the paying seats'' burden.').

omega_variable(
    metaphysical_load_bearing_independence,
    'Was the mathematical exclusion load-bearing for the underlying ex nihilo metaphysics, or did the metaphysics survive zero''s admission unchanged?',
    'Trace the ex nihilo arguments across the adoption threshold: if they proceed identically before and after zero became a number, the mathematical arrangement was not their foundation.',
    'If independent, the coordination function claimed for the exclusion was narrower than advertised and a larger share of its measured cost was overhead rather than the price of coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_load_bearing_independence, conceptual, 'Whether the metaphysics needed the arithmetic exclusion at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zms_parm_rejection_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.05).
narrative_ontology:measurement(zms_parm_rejection_tr_t1, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1, 0.08).
narrative_ontology:measurement(zms_parm_rejection_tr_t5, zero_mathematical_status__parmenidean_rejection, theater_ratio, 5, 0.15).
narrative_ontology:measurement(zms_parm_rejection_tr_t8, zero_mathematical_status__parmenidean_rejection, theater_ratio, 8, 0.2).
narrative_ontology:measurement(zms_parm_rejection_tr_t12, zero_mathematical_status__parmenidean_rejection, theater_ratio, 12, 0.28).
narrative_ontology:measurement(zms_parm_rejection_tr_t16, zero_mathematical_status__parmenidean_rejection, theater_ratio, 16, 0.38).
narrative_ontology:measurement(zms_parm_rejection_tr_t18, zero_mathematical_status__parmenidean_rejection, theater_ratio, 18, 0.52).
narrative_ontology:measurement(zms_parm_rejection_tr_t20, zero_mathematical_status__parmenidean_rejection, theater_ratio, 20, 0.7).
narrative_ontology:measurement(zms_parm_rejection_tr_t22, zero_mathematical_status__parmenidean_rejection, theater_ratio, 22, 0.85).

% Extraction over time
narrative_ontology:measurement(zms_parm_rejection_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(zms_parm_rejection_be_t1, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1, 0.25).
narrative_ontology:measurement(zms_parm_rejection_be_t5, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(zms_parm_rejection_be_t8, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(zms_parm_rejection_be_t12, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(zms_parm_rejection_be_t16, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(zms_parm_rejection_be_t18, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(zms_parm_rejection_be_t20, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(zms_parm_rejection_be_t22, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 22, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(zms_parm_rejection_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(zms_parm_rejection_su_t1, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1, 0.3).
narrative_ontology:measurement(zms_parm_rejection_su_t5, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(zms_parm_rejection_su_t8, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(zms_parm_rejection_su_t12, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(zms_parm_rejection_su_t16, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(zms_parm_rejection_su_t18, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(zms_parm_rejection_su_t20, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(zms_parm_rejection_su_t22, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 22, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'zero's mathematical status' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file instantiates parmenidean_rejection — the exclusion arrangement, epsilon 0.72 at its peak, victims among computational practitioners, actively enforced, historically dominant from the Eleatics through the medieval faculties. zero_mathematical_status__number_reading instantiates the inclusive arrangement (Brahmagupta's rules canonical; negligible extraction, near-rope profile). zero_mathematical_status__placeholder_reading instantiates the notation-only compromise (efficiency gains granted for blanks but withheld from arithmetic; intermediate extraction). The rejection reading is historically upstream: its dominance shaped the conditions under which the placeholder compromise was the maximum concession available, and its collapse is the number_reading's victory condition. Each story carries its own beneficiaries, victims, and fate; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
