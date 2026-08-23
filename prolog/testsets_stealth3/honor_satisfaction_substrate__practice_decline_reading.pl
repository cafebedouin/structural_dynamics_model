% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor-Satisfaction Substrate under Suppressed Practice (Practice-Decline Reading)
 *   domain: historical sociology/cultural anthropology/legal history
 *
 * SUMMARY:
 *   This story instantiates the practice_decline_reading of the
 *   honor-satisfaction kernel: the code of honor persists as a normative
 *   substrate — defining what counts as an insult, what response it demands,
 *   and what refusal costs — while the dueling practice itself declined
 *   because exogenous enforcement closed the practice channel: statutory
 *   prohibition, military and professional barriers, prosecution of
 *   principals and seconds, and rising opportunity cost of the gentleman's
 *   time. On this reading dueling remains thinkable but impractical, and the
 *   code survives in attenuated institutional forms (officer honor codes,
 *   Southern culture-of-honor reputation systems) rather than having been
 *   internally transformed or dying. The file belongs to a linked family
 *   decomposing the colloquial label 'why dueling declined' into separate
 *   constraint stories; the family relationships are recorded in
 *   network.dual_formulation_note and commentary.kernel_context. KEY AGENTS
 *   (by structural relationship): - aristocratic_gentry_class: primary
 *   beneficiary and the code's administrator (powerful/identity_locked) —
 *   collects status assurance from the shared standard, wrote and policed the
 *   protocol - military_officer_corps: attenuated-form beneficiary
 *   (organized/identity_locked) — inherits the code as professional ethos
 *   after the pistol left the table - southern_honor_communities: dual
 *   inheritor seat (organized/identity_locked) — collects deterrence-based
 *   order, pays in retaliatory violence - challenged_gentlemen: primary
 *   target (moderate/trapped) — bears the code's sharpest cost: fight,
 *   refuse, or be ruined - excluded_laboring_classes: boundary victims
 *   (powerless/trapped) — outside the code's protection and its obligations,
 *   insulable without protocol - state_courts_and_legislatures: exogenous
 *   enforcer become de facto administrator of the practice channel
 *   (institutional/constrained) - historical_sociologists_of_honor:
 *   analytical observer — sees substrate persistence and practice suppression
 *   as separable variables
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.45).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.35).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor-Satisfaction Substrate under Suppressed Practice (Practice-Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical sociology/cultural anthropology/legal history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3').
narrative_ontology:cs_kernel_codification('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', distributed).
narrative_ontology:cs_authority_grounding('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', practice).
narrative_ontology:cs_interpretation_layer_present('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3').
narrative_ontology:cs_reading_relation('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', foundational, exogenous_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(exogenous_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', exogenous_enforcement_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', foundational, substrate_continuity_under_suppression).
narrative_ontology:cs_axiom_status(substrate_continuity_under_suppression, holdable).
narrative_ontology:cs_axiom_grounding('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', substrate_continuity_under_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', intact_honor_code_practice_ready).
narrative_ontology:cs_drift_state('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', post_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('669b2f21-ed8b-4bc5-b786-f6e7ec7b73b3', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, aristocratic_gentry_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, southern_honor_communities).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, challenged_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, excluded_laboring_classes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, southern_honor_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A transnational class of titled and untitled gentlemen whose standing depended on a shared code: what counts as an insult, what response it demands, and what refusal marks a man as. Through seconds, printed dueling codes, and informal courts of honor the class wrote and administered the protocol, admitted and expelled members, and collected the standing that came from belonging. Leaving the code was not a menu option — a gentleman who renounced the satisfaction obligations ceased, socially, to be a gentleman. The class also paid: its sons died on the field of honor, and its members faced prosecution once the laws turned.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, aristocratic_gentry_class, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, aristocratic_gentry_class, agenda_setter).

% Officer bodies across Europe and America carried the code into professional life: commissions depended on reputation, and regimental culture treated an unanswered slur as disqualifying. After national governments made dueling a career-ending offense, the corps kept the underlying ethic — an officer's word, the defense of unit reputation, formal honor boards — while the pistol left the table. The institution gains cohesion and a professional ethos from the inherited code; it also spends disciplinary effort policing the conduct the code still stirs up.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    organized, generational, identity_locked, national).

% The man who receives a challenge faces the code's sharpest edge: fight and risk death, prosecution, and ruin, or refuse and wear cowardice before everyone whose opinion governs his life. For most of the interval there was no dignified way out of this bind; only late, when legal prohibition gave respectable cover, did declining stop costing everything. His fate was the enforcement mechanism the class relied on — every ruin and every funeral taught the next man the code's price.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, challenged_gentlemen, payer,
    moderate, immediate, trapped, national).

% Those outside the honor class could give no challenge and demand no satisfaction. The code's protections and its obligations stopped at the class line, which meant a gentleman could insult a laborer with no protocol due and no standing forfeited. The boundary the code drew around whose reputation mattered is a cost borne daily by those on the far side of it, and they had no seat in the proceedings that drew it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, excluded_laboring_classes, payer,
    powerless, generational, trapped, national).

% Communities — historically in the American South and comparable frontier-herding settings — where reputation operates as the working deterrent: an unanswered slight invites the next one, so men answer. The inherited honor logic orders social life without heavy formal policing, and members collect the order it provides; they also pay in elevated retaliatory violence and in the narrowness of what counts as a respectable response to conflict. Membership runs kinship-deep; stepping out of the honor frame means stepping out of the community.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, southern_honor_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, southern_honor_communities, payer).

% Legislatures criminalized the duel, military law made it a dismissable offense, and courts prosecuted principals and seconds alike; insurers and professions stacked on their own bars. Over the interval the state moved from tolerating the practice to defining which responses to an insult remain lawful — by the interval's end it is the effective administrator of the practice channel the honor code once ran. It spends enforcement effort and collects a monopoly over dispute resolution in exchange.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_courts_and_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Scholars of honor cultures, dueling's decline, and violence norms reconstruct the whole structure from archives, trial records, and comparative regional data. They can hold the code's persistence and the practice's suppression as separate variables — the distinction the entire explanatory dispute turns on — and hold no stake in which account wins.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historical_sociologists_of_honor, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code solved a collective-action problem for the honor class: how to make reputation credible and deter predation on members' standing where state dispute resolution was weak, slow, or partial. Standardized insult-and-satisfaction protocols converted private quarrels into rule-governed proceedings — challenges delivered through seconds, published codes of procedure, settlement paths short of combat — so that deterrence worked without a state monopoly on enforcement.
% TRANSFER_FUNCTION: Moves life-and-limb risk, time, and standing from individual gentlemen (who must answer challenges or lose caste) to the honor class collectively (whose shared standard the payments uphold); moves resolution authority from state courts to private negotiation between seconds; and moves immunity across the class line, shielding gentlemen's standing while leaving those outside the code without protocol or protection.
% ABSENT_VOICES: The excluded laboring classes would object that the code licensed impunity for insults against them while commanding deference to gentlemen's reputations; they were outside the rooms where the protocol was written. Religious authorities condemned the practice for centuries and were heard but consistently outvoted within the class. Women who bore the bereavement and ruin the protocol produced had no seat in any affair of honor.
% DISAPPEARANCE_RATIONALE: If the substrate vanished overnight, arrangements would visibly move: officer professional ethics would lose one of its historical load-bearing strands (the residue of the officer's word and unit reputation), Southern and comparable honor communities would lose their deterrence-reputation equilibrium and renegotiate conflict norms toward either formal-legal or different informal orders, and elite dispute practice would complete its migration into legal process. The world would not collapse — the state now handles what the code once did — but the surviving normative architecture these groups run on would have to be rebuilt or consciously replaced.
% FOUNDING_PROBLEM: Pre-modern elites operated in settings where state protection of personal standing was weak, partial, or captured by rivals: a gentleman's safety and rank depended on credible personal deterrence. The code of honor with regulated satisfaction was built to make that personal deterrence reliable, rule-bound, and class-bounded.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties, and it cuts both ways, which is why the status is authored contested rather than asserted. Legal-historical scholarship on defamation law and the state's absorption of dispute resolution attests that the founding problem — weak-state personal deterrence for elites — is dead in consolidated states. Sociology of honor cultures (the herding-frontier and retaliation-norm literature) attests that deterrence-through-reputation remains a live ordering mechanism where formal enforcement is thin, supporting the beneficiaries' claim that the problem persists in attenuated settings. Church condemnations and anti-dueling societies corroborate, from outside the beneficiary set, that the founding rationale was disputed even at the code's height. No single outside source settles the dispute; the disagreement itself is the attested finding.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the standing arrangement still obligates and costs its holders — response duties, status discipline, retaliatory expectations in honor communities — but the lethal channel is closed and the attenuated forms impose real yet bounded costs; the reading's core claim is persistence, so the series plateaus rather than collapsing to zero. Suppression 0.35: compliance with the surviving substrate rests mostly on identity and habituation, not active coercion; notably, legal prohibition supplied gentlemen a respectable exit ('the law forbids me') that lowered the internal coercion the code itself had to apply. Theater_ratio 0.40: a growing share of the apparatus is ritual — academic fencing with protective gear, first-blood settlements managed by seconds, staged political affairs — but the functional core (professional integrity codes, community deterrence) remains real, so the ratio stays below the performative-majority threshold. Accessibility_collapse 0.35: alternatives to answering an insult remain open and multiplied after prohibition (litigation, public mockery, ignoring, legal recourse) — the reading's whole point is that exogenous substitutes existed and worked. Resistance 0.55: the code met sustained organized opposition throughout — religious condemnation, anti-dueling societies, satirists, reforming legislatures — which is part of the causal story this reading tells. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the FALLING trajectory models the decay of the substrate's own internal coercive machinery as the state absorbed the enforcement function — displacement, not mere relaxation. All three series run on one shared time grid ({0,25,50,75,100,125,150}) so every metric is authored at every examined point; endpoint values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the gentry seat the code is constitutive rather than constraining — it is what being a gentleman IS, so the arrangement reads as order, not imposition. From the challenged_gentlemen seat the same protocol is a trap with no dignified exit for most of the interval. From the state seat the object in view is a practice to be extinguished, not a living normative system — the state's records see prosecutions and statutes, not the substrate underneath. From the excluded_laboring_classes seat the code appears as a boundary drawing who matters, experienced as impunity for insults against them. The observer seat alone holds substrate-persistence and practice-suppression as separate variables, which is precisely the distinction the explanatory dispute turns on.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the collecting seats: the gentry class sits nearest the beneficiary pole (identity-locked collector with no exit), while the officer corps and southern honor communities derive moderately low d because each pays real costs on the same ledger it collects from (disciplinary rigidity; retaliatory violence). Victim declarations drive high d: challenged_gentlemen sit nearest the target pole (trapped — the fight-or-ruin bind defined the era), and excluded_laboring_classes sit high as well, bearing the boundary the code draws even though they pay no compliance costs. The state seat derives near-symmetric: it spends enforcement effort and gains dispute-resolution monopoly in rough balance. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Receipt surface: gain_flow is authored as 'diffuse' as an affirmative checked claim — at the interval end the gentry class has dissolved as a capturer, the officer corps and honor communities each receive benefits and pay costs on the same ledger, and no named seat demonstrably nets the extraction; the check was performed seat by seat against the situations above. fixing_cost is authored 'prohibitive' on its own evidence: for whoever could finish the removal the states and institutions began, full extirpation would cost more than the residual harm warrants, chiefly because the substrate still performs coordination work whose loss would be part of the price of fixing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing two symmetrical mislabels. Against the dead-relic mislabel: the substrate is not mostly performance — theater_ratio 0.40 with a functional core of professional-ethics and community-deterrence work, and the extraction series plateaus rather than decaying to noise, which is what a merely vestigial arrangement would show. Against the hidden-tyranny mislabel: no seat captures the gains (receipt surface 'diffuse', affirmatively checked), the internal coercive machinery measurably decayed as law absorbed enforcement, and alternatives to the code's response duties remain open. The rope claim keeps the genuine coordination function visible: the code solved a real collective-action problem for its class and its attenuated successors still solve successor problems. The genealogy interview is authored to keep the live question open rather than to close it: founding_problem_status 'contested' combined with disappearance_verdict 'world_rearranges' is a deliberate mismatch flag — the arrangement outlived its founding problem (weak-state personal deterrence for elites) while successor functions keep it coordinated. That mismatch routes to investigation rather than verdict, which is the honest state for a substrate whose persistence mechanism (identity fusion plus successor functions) is exactly what this reading asserts and its siblings deny or divide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the practice_decline_reading of the honor_satisfaction_substrate kernel; would instantiating cultural_contraction_reading or composite_overdetermined_reading instead change the constraint''s beneficiary/victim structure and epsilon?',
    'Author the sibling stories as separate constraints and compare derived structures: under cultural_contraction_reading the surviving norms are dignity-norms rather than attenuated honor, so the old substrate''s victim set empties and its epsilon falls toward zero (a dead constraint, not an attenuated one); under composite_overdetermined_reading epsilon and persistence split across two interacting mechanisms with non-independent pathways.',
    'Sibling selection changes whether the standing arrangement contains a live honor substrate at all: this reading''s rope classification depends on the substrate surviving intact beneath the suppressed practice; the contraction sibling replaces it with a different normative system, and the composite sibling divides the persistence claim between mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the honor-satisfaction kernel this constraint instantiates and what siblings would change.').

omega_variable(
    endogenous_exogenous_separability,
    'Can the decline of the dueling practice be causally attributed to exogenous enforcement alone (statutes, military regulations, prosecution, opportunity cost), or did endogenous normative change operate concurrently such that the two cannot be separated?',
    'Comparative analysis across jurisdictions holding the honor substrate constant while varying enforcement timing and intensity (Britain vs. continental regimes vs. American regions): if practice declined uniformly regardless of enforcement variation, endogenous transformation dominates; if decline tracks enforcement, the exogenous account holds.',
    'If endogenous transformation dominates, this reading''s core claim (substrate intact, practice blocked) misdescribes the survivor and the classification migrates toward the contraction sibling''s constraint; if enforcement dominates, the rope classification and the persistence measurements stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_exogenous_separability, empirical, 'Whether exogenous enforcement is causally separable from endogenous delegitimation in the practice''s decline.').

omega_variable(
    substrate_continuity_vs_descendant_lineages,
    'Are military honor codes and Southern culture-of-honor genuinely the SAME constraint as the dueling-era substrate in attenuated operation, or distinct descendant institutions that merely share ancestry?',
    'Trace normative-content continuity (insult-response obligation, reputation-deterrence logic, satisfaction protocol structure) against mere lexical or ceremonial survival, using archival chains from dueling codes through academy honor codes and community sanction records.',
    'If the attenuated forms are distinct descendants, the persisting original substrate is thinner than this reading claims, its attributable epsilon falls, and the original drifts toward vestigial-inertia classification; if continuous, the rope claim and the persistence plateau in the measurement series stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_continuity_vs_descendant_lineages, conceptual, 'Whether attenuated survivals are the same constraint diminished or new constraints with common ancestry.').

omega_variable(
    internal_coercion_decay_or_displacement,
    'Did the honor substrate''s internal coercive machinery (compulsory response, social ruin for refusal) genuinely decay, or did it displace into new channels such as professional discipline boards, academy honor systems, and community sanction?',
    'Compare sanction incidence and severity for honor-relevant violations inside officer corps and honor communities before and after legal prohibition, controlling for institutional formalization.',
    'If displaced, effective suppression is higher than the scalar suggests and the substrate retains sharp enforcement teeth in institutional pockets, pulling classification toward enforced hybrid forms; if decayed, the low suppression value and rope classification solidify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_coercion_decay_or_displacement, empirical, 'Decay versus displacement of the substrate''s internal coercive machinery after prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hono_tr_t25, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement(hono_tr_t50, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(hono_tr_t75, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 75, 0.31).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement(hono_tr_t125, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 125, 0.39).
narrative_ontology:measurement(hono_tr_t150, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(hono_be_t25, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(hono_be_t50, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(hono_be_t75, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 75, 0.5).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 100, 0.47).
narrative_ontology:measurement(hono_be_t125, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 125, 0.46).
narrative_ontology:measurement(hono_be_t150, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 150, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(hono_su_t25, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(hono_su_t50, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hono_su_t75, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 75, 0.5).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 100, 0.43).
narrative_ontology:measurement(hono_su_t125, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 125, 0.38).
narrative_ontology:measurement(hono_su_t150, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 150, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, dueling_prohibition_enforcement).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why dueling declined' conflates structurally distinct claims and is decomposed into a three-story family over the honor_satisfaction_substrate kernel. This file instantiates the practice_decline_reading (substrate intact, practice suppressed exogenously, epsilon moderate over the persisting arrangement). The sibling cultural_contraction_reading authors a different constraint: a transformed normative system in which the old substrate's victim set has emptied and its epsilon falls toward zero. The sibling composite_overdetermined_reading authors the joint mechanism with non-independent pathways. The upstream enforcement regime (dueling_prohibition_enforcement) structurally influences this constraint — it is the exogenous force this reading identifies as the cause of practice decline — and each reading links its siblings per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
