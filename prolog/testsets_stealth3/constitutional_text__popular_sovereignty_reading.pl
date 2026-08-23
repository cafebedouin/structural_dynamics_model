% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Constitutional Authority
 *   domain: political/legal
 *
 * SUMMARY:
 *   This story instantiates the popular_sovereignty_reading of the
 *   constitutional_text kernel: the text's authority derives from the
 *   constituent power of the demos, and neither courts nor legislature hold
 *   final interpretive authority — the people retain it through amendment,
 *   convention, or revolution. As a standing arrangement it operates as a
 *   hybrid: it performs a genuine coordination function (keeping the text's
 *   bindingness re-derivable from the governed, solving the
 *   obedience-to-the-dead problem) while imposing asymmetric, actively
 *   enforced costs on institutional actors whose outputs remain permanently
 *   revisable. Constraint-family note: the colloquial label 'constitutional
 *   authority' decomposes into three structurally distinct readings of one
 *   kernel — judicial_supremacy_reading (final say with courts; extraction
 *   concentrated on litigants and popular movements),
 *   legislative_sovereignty_reading (final say with parliament; extraction
 *   concentrated on courts and popular override channels), and this reading
 *   (final say with the demos; extraction concentrated on institutional
 *   stability and expertise). Each file carries its own epsilon,
 *   beneficiaries, and victims; they are linked via
 *   network.affects_constraints, not merged. The claim/metric relationship is
 *   deliberate: claimed_type records the structural judgment (hybrid
 *   coordination/extraction), while the metrics describe observed operation
 *   independently.
 *
 * KEY AGENTS:
 *   - - the_constituent_demos: Agenda-setting seat ([organized]/[constrained]) — holds ultimate interpretive authority, exercises it rarely through amendment and convention channels
 *   - - popular_sovereignty_movements: Primary beneficiary ([organized]/[mobile]) — gain standing when mobilization forces institutional retreat or wins amendments
 *   - - amendment_advocacy_coalitions: Secondary beneficiary ([organized]/[mobile]) — convert grievance into formal constitutional change
 *   - - federal_state_ratifying_bodies: Gatekeeping beneficiary ([institutional]/[constrained]) — hold ratification vetoes over proposed changes
 *   - - constitutional_courts: Primary target ([powerful]/[constrained]) — issue rulings that remain permanently reversible by the override channels
 *   - - national_legislatures: Dual-positioned target ([institutional]/[constrained]) — statutes subject to supersession, yet gatekeep amendment proposal
 *   - - insulated_expert_agencies: Target ([moderate]/[constrained]) — technocratic discretion eroded by override threats
 *   - - counter_majoritarian_minority_groups: Excluded voice ([powerless]/[trapped]) — protected on paper, unable to sit in the constituent conversation
 *   - - comparative_constitutional_scholars: Analytical observer ([analytical]/[analytical]) — track amendment rates, convention movements, and legitimacy outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.28).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Popular Sovereignty Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "political/legal").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '4d29bf97-29a5-4dc6-ac01-beafa253606d').
narrative_ontology:cs_kernel_codification('4d29bf97-29a5-4dc6-ac01-beafa253606d', fixed_text).
narrative_ontology:cs_authority_grounding('4d29bf97-29a5-4dc6-ac01-beafa253606d', lineage).
narrative_ontology:cs_reading_relation('4d29bf97-29a5-4dc6-ac01-beafa253606d', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4d29bf97-29a5-4dc6-ac01-beafa253606d', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('4d29bf97-29a5-4dc6-ac01-beafa253606d', foundational, demos_retains_constituent_supremacy).
narrative_ontology:cs_axiom_status(demos_retains_constituent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4d29bf97-29a5-4dc6-ac01-beafa253606d', demos_retains_constituent_supremacy, deontological).
narrative_ontology:cs_axiom('4d29bf97-29a5-4dc6-ac01-beafa253606d', foundational, override_channels_are_authoritative_expression).
narrative_ontology:cs_axiom_status(override_channels_are_authoritative_expression, holdable).
narrative_ontology:cs_axiom_grounding('4d29bf97-29a5-4dc6-ac01-beafa253606d', override_channels_are_authoritative_expression, conventional).
narrative_ontology:cs_axiom('4d29bf97-29a5-4dc6-ac01-beafa253606d', secondary, institutional_interpretation_is_contingent_acquiescence).
narrative_ontology:cs_axiom_status(institutional_interpretation_is_contingent_acquiescence, holdable).
narrative_ontology:cs_axiom_grounding('4d29bf97-29a5-4dc6-ac01-beafa253606d', institutional_interpretation_is_contingent_acquiescence, instrumental).
narrative_ontology:cs_reference_frame('4d29bf97-29a5-4dc6-ac01-beafa253606d', constituent_power_primacy).
narrative_ontology:cs_drift_state('4d29bf97-29a5-4dc6-ac01-beafa253606d', contemporary_amendment_dormancy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d29bf97-29a5-4dc6-ac01-beafa253606d', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_constituent_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_sovereignty_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, amendment_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, federal_state_ratifying_bodies).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, national_legislatures).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, insulated_expert_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, national_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body of citizens in whom the text's authority originates and terminates. Formally holds the power to reinterpret the constitution by amendment, by calling a convention, or in the limit by revolution; in practice convenes rarely, delegating day-to-day interpretation to institutions while retaining the standing ability to reverse them. Bears the mobilization and deliberation costs of exercising that power, and the risk that majoritarian exercises override protections some of its own members rely on. Exit would mean renouncing citizenship or secession — neither realistic for most members.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_constituent_demos, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, the_constituent_demos, beneficiary).

% Organized campaigns that arise when courts or legislatures are perceived to have displaced popular judgment — term-limits drives, convention-application campaigns, amendment movements. They gain standing and policy wins when mobilization forces institutional retreat or clears amendment thresholds, and they dissolve or hibernate between waves. Disbanding is easy; influence depends entirely on periodic re-mobilization.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_sovereignty_movements, beneficiary,
    organized, biographical, mobile, national).

% Issue-specific coalitions — campaign-finance reformers, fiscal-rule advocates, historical suffrage extensions — that convert grievance into formal textual change. Successes are rare because the threshold is high, but wins are durable and authoritative in a way statutory wins are not. Between attempts they operate as ordinary interest groups.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, amendment_advocacy_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Subnational legislatures whose assent is required for amendments in federal systems. They collect gatekeeping power: any proposed change must clear their chambers, giving sparsely populated units disproportionate weight in the constituent process. They cannot initiate most changes themselves but can block nearly all of them.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, federal_state_ratifying_bodies, beneficiary,
    institutional, generational, constrained, regional).

% Judges who decide concrete cases under the text while knowing any doctrinal line they draw can be reversed by amendment or swept aside by a convention. Their rulings carry day-to-day force, but final-say pretensions are precisely what the arrangement denies; several landmark reversals in the historical record came at the hands of the amendment channel. Exit would mean resignation or jurisdiction-stripping fights — staying and moderating their claims is the realistic posture.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_courts, payer,
    powerful, biographical, constrained, national).

% Statute-makers whose products remain subject to supersession by referendum, convention, or amendment, and whose long-term programs can be undone by a later mobilized majority. At the same time they usually control the proposal stage of the amendment process, giving them agenda leverage over the very channel that disciplines them. They pay in exposed programs and collect in gatekeeping.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, national_legislatures, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, national_legislatures, beneficiary).

% Technocratic bodies — central banks, regulatory commissions, professional standard-setters — whose effectiveness depends on insulation from electoral swings. Override threats erode that insulation: rules they issue can be targeted by amendment campaigns, and their discretion is discounted accordingly. They hold no seat in the constituent process and no exit from its reach.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, insulated_expert_agencies, payer,
    moderate, biographical, constrained, national).

% Groups that depend on durable supermajority consensus for protection — religious minorities, dispersed ethnic minorities, unpopular political movements. The arrangement promises that entrenched rights can only be changed through the same supermajority channels, but they hold little presence in amendment politics and watch majoritarian waves with alarm. They are governed by the constituent power but largely absent from its exercise.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, counter_majoritarian_minority_groups, excluded,
    powerless, biographical, trapped, national).

% Academics and comparativists who study whether popular-sovereignty arrangements sustain legitimacy better than court-centered or parliament-centered ones. They count amendment rates, convention applications, and reversal episodes; they publish from outside every seat and hold no stake in the outcome beyond disciplinary standing.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, the_constituent_demos).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legitimacy problem of written constitutionalism: why the living should treat a text authored by the dead as binding. By locating constituent power permanently in the demos, the arrangement makes the constitution's authority continuously re-derivable from the governed's retained power to remake it, and channels disputes over ultimate authority into amendment and convention procedures instead of recurring constitutional crisis.
% TRANSFER_FUNCTION: Moves final-say status and agenda-setting power over constitutional meaning away from courts and legislatures toward mobilized popular majorities; moves the costs of perpetual contestability — chilled rulings, discounted long-term planning, eroded technocratic insulation — onto institutional actors; moves mobilization and deliberation costs onto citizens.
% ABSENT_VOICES: Counter-majoritarian minorities, disenfranchised residents governed by the text but outside the demos (territorial populations, non-citizen residents), and future generations bound by ratifications they never joined would object that constituent power can reach their protections while they hold no seat in amendment or convention politics. They sit outside the franchise and appear in the arrangement mainly as litigation subjects, not as constituents.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — if no seat, popular or institutional, held ultimate interpretive authority — the text's bindingness would lose its legitimacy ground: courts and legislatures would immediately contest supremacy (the sibling readings would rush into the vacuum), amendment coalitions would lose their authoritative channel, and either a new supremacy claim would consolidate or the constitution would degrade into ordinary statute. Every actor's position depends on the settled answer to who commands the text.
% FOUNDING_PROBLEM: How a free people can bind itself to a written constitution across generations without thereby installing a new master — the constituent-power problem articulated at the founding: if courts or parliaments are made supreme, the governed are ruled by an institution; if nothing is supreme, the text dissolves. The arrangement was built to keep the binding force of the text compatible with continued self-government.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional courts themselves (victim-seat actors) acknowledge constituent supremacy in doctrine — amendment-power jurisprudence and the German eternity-clause debate both presuppose a demos that could in principle reconstitute; comparative scholars (Ackerman, Tushnet, Levinson, Albert) document the founding problem across jurisdictions; and the historical record of amendments reversing apex-court rulings (the Eleventh, Fourteenth, Sixteenth, and Twenty-Sixth Amendments in the US case) attests that the problem recurs whenever institutional interpretation diverges from durable popular will.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) and concentrated on institutional seats: courts price in reversibility, legislatures discount long-horizon programs, agencies lose insulation — but the burden is bounded by supermajority amendment thresholds, and the receiving side is diffuse rather than capturing. Suppression (0.28) is a raw structural property, unscaled by the engine: it reflects the normative foreclosure of rival supremacy claims inside the arrangement, weakened by decades of enforcement decay. Theater_ratio (0.38) is the sharpest signal: as amendment usage collapsed (one ratification in roughly fifty years in the US case), invocation of popular sovereignty migrated from practice toward rhetoric — preamble recitation, convention threats that never convene. Accessibility_collapse is low (0.30): the sibling readings remain fully constructible from the same text, which is why resistance (0.58) is real — courts assert supremacy in practice, legislatures entrench, scholars defend rival frames. The temporal series shares one grid; the extraction hump tracks mobilization waves (Progressive Era, 1960s–70s amendments) rather than a strict cycle, and the series was measured at end-of-wave quiescence.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement reads as dispossession: a court's ruling, an agency's rule, a legislature's program can be swept by a mobilized majority that never argued the merits — institutional actors experience permanent exposure and rationally discount counter-majoritarian functions like minority-rights protection. From the beneficiary seats the identical structure is the guarantee that keeps government answerable: the same reversibility that chills courts empowers movements. The agenda-setter seat is peculiar — the demos formally occupies it but almost never convenes, so the seat is filled in practice by rhetoric and by the memory of past exercises. The engine computes these per-seat classifications from the structural data; the divergence between the court seat and the movement seat is the measurement, not noise. Coalition note: the powerless excluded seat (counter-majoritarian minorities) has a theoretical path into the beneficiary set — the override channels are open to any coalition that can clear supermajority thresholds — but collective-action costs have kept that path effectively closed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the demos (agenda-setter and residual beneficiary) sits nearest the subsidy end — the arrangement exists to preserve its authority; movements and coalitions collect standing episodically; ratifying bodies collect gatekeeping advantages. Victim declarations map to high directionality: courts, legislatures, and expert agencies bear the transfer with constrained exit — none can leave the constitutional order they are subordinate within, and their power does not purchase exemption (a powerful court is precisely what the constraint targets). Legislatures are the one genuinely dual seat: they pay in superseded statutes yet collect agenda power as proposal gatekeepers, which moderates their effective extraction below a pure-payer read. Scope is national: verifying what the people actually want is hard at national scale, which modestly amplifies effective extraction on the targeted seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Reading the arrangement as pure rope would erase the named victims — institutional stability and expertise genuinely pay, and the payment is enforced, not voluntary. Reading it as snare would erase the coordination function: the obedience problem the arrangement solves is real, corroborated from outside the beneficiary set, and unresolved. The founding problem remains live (texts age, institutions drift, mobilization recurs), so the mandate has not outlived its function and no mandatrophy resolution is declared. The live risk is not obsolescence but drift: rising theater_ratio alongside falling suppression_requirement traces a constraint whose enforcement is decaying faster than its rhetoric — the dormancy omega tracks whether periodic mobilization continues to re-fund the function or whether the arrangement completes its slide into preamble recitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_constitutional_text_kernel,
    'This constraint is one reading of the constitutional_text kernel (popular_sovereignty_reading). What structural deltas would the sibling readings produce if instantiated instead?',
    'Cross-reading comparison of the sibling files'' victim sets, epsilon values, and seat classifications; convergence tests on shared stakeholders (constitutional_courts, national_legislatures).',
    'Under judicial_supremacy_reading the victim set shifts to popular movements and review subjects and the beneficiary seat becomes the court; under legislative_sovereignty_reading the victim set shifts to courts and popular override channels and the beneficiary seat becomes parliament. Classification of this file is unaffected; the deltas locate the disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_constitutional_text_kernel, conceptual, 'Committer structure: one of three readings of the constitutional_text kernel; siblings relocate final authority and invert the victim set.').

omega_variable(
    amendment_dormancy_piton_drift,
    'Is the arrangement drifting toward inertial maintenance — popular sovereignty preserved rhetorically while the override channels go unused — or do periodic mobilization waves keep the coordination function funded?',
    'Track amendment ratification rates, convention-application counts, and successful popular overrides of institutional interpretations over successive decades; sustained zero-exercise with rising ceremonial invocation indicates piton drift.',
    'Confirmed dormancy would push reclassification toward piton (theatrical maintenance of an atrophied function); renewed exercise (a called convention, a rights-expanding amendment wave) confirms the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_dormancy_piton_drift, empirical, 'Whether rising theater_ratio signals terminal atrophy or normal inter-wave quiescence.').

omega_variable(
    revolution_channel_membership,
    'Is revolution part of the standing arrangement''s override channels or a background possibility outside it?',
    'Doctrinal analysis of how the reading''s own tradition treats revolutionary override — operative channel (as at the founding) versus extra-constitutional limit case; test against whether the arrangement''s enforcement calculus prices existential override.',
    'Including revolution raises the suppression profile (institutions face unbounded override) and strengthens the enforcement requirement; excluding it narrows the channels to amendment and convention and lowers effective institutional exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolution_channel_membership, conceptual, 'Boundary of the override-channel set: does the constraint''s authority claim extend to extralegal constituent action?').

omega_variable(
    demos_boundary_underdetermination,
    'Who counts as ''the people'' whose constituent power anchors the arrangement?',
    'Comparative analysis of inclusion rules across amendment and convention mechanisms: citizenship requirements, territorial representation, enfranchisement of governed non-members.',
    'A narrower demos raises effective extraction on governed-but-excluded populations (they bear the text without constituent standing); a broader demos dilutes the beneficiary concentration and lowers per-seat extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_underdetermination, conceptual, 'Beneficiary-set boundary: the demos is constitutive of the arrangement but its membership is not settled by it.').

omega_variable(
    accountability_vs_extraction_ambiguity,
    'Are the burdens on courts and expertise extraction (chilled counter-majoritarian protection, discounted long-horizon governance) or the legitimate price of democratic accountability?',
    'Outcome comparison across regimes: rights protection and administrative continuity in popular-sovereignty-weighted systems versus court-supremacy systems, controlling for wealth and democracy indices.',
    'If institutional burdens buy accountability without degrading protection, epsilon falls toward rope; if they measurably chill minority protection and planning, the tangled_rope reading firms and snare-drift risk rises during mobilization waves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_vs_extraction_ambiguity, preference, 'Valence of the institutional burden: accountability dividend or extraction cost — partly a values question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_text__popular_sovereignty_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_text__popular_sovereignty_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement_basis(cons_tr_t120, observed).
narrative_ontology:measurement(cons_tr_t160, constitutional_text__popular_sovereignty_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement_basis(cons_tr_t160, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_text__popular_sovereignty_reading, theater_ratio, 200, 0.33).
narrative_ontology:measurement_basis(cons_tr_t200, observed).
narrative_ontology:measurement(cons_tr_t240, constitutional_text__popular_sovereignty_reading, theater_ratio, 240, 0.38).
narrative_ontology:measurement_basis(cons_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_text__popular_sovereignty_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_text__popular_sovereignty_reading, base_extractiveness, 120, 0.52).
narrative_ontology:measurement_basis(cons_be_t120, observed).
narrative_ontology:measurement(cons_be_t160, constitutional_text__popular_sovereignty_reading, base_extractiveness, 160, 0.48).
narrative_ontology:measurement_basis(cons_be_t160, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_text__popular_sovereignty_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement_basis(cons_be_t200, observed).
narrative_ontology:measurement(cons_be_t240, constitutional_text__popular_sovereignty_reading, base_extractiveness, 240, 0.42).
narrative_ontology:measurement_basis(cons_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_text__popular_sovereignty_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(cons_su_t80, observed).
narrative_ontology:measurement(cons_su_t120, constitutional_text__popular_sovereignty_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement_basis(cons_su_t120, observed).
narrative_ontology:measurement(cons_su_t160, constitutional_text__popular_sovereignty_reading, suppression_requirement, 160, 0.4).
narrative_ontology:measurement_basis(cons_su_t160, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_text__popular_sovereignty_reading, suppression_requirement, 200, 0.32).
narrative_ontology:measurement_basis(cons_su_t200, observed).
narrative_ontology:measurement(cons_su_t240, constitutional_text__popular_sovereignty_reading, suppression_requirement, 240, 0.28).
narrative_ontology:measurement_basis(cons_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'constitutional authority' covers three structurally distinct claims that share one kernel (constitutional_text) but differ in who holds the ultimacy slot. This file instantiates the popular_sovereignty_reading; judicial_supremacy_reading and legislative_sovereignty_reading are separate stories with their own epsilon values, beneficiary/victim structures, and classifications. The upstream/downstream structure runs through shared stakes: whichever reading consolidates in practice changes the operating environment of the other two (court assertion of finality pressures the popular channel; dormant amendment channels invite judicial consolidation). Linkage is via network.affects_constraints; no reading's epsilon is averaged into another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
