% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Erosion Reading)
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   The honor-satisfaction mechanism is the standing arrangement under which
 *   a gentleman's injured honor could be satisfied only by risking his life
 *   in a rule-governed duel, refusal carrying social and career death. This
 *   file instantiates the COMPOSITE reading of the contested kernel: the
 *   arrangement was dismantled by four independent pressures — the state's
 *   monopolization of grievance adjudication and legitimate violence, the
 *   bourgeois normative economy of prudence and creditworthiness, the
 *   actuarial reframing of duel death as uninsurable imprudence, and a
 *   category-shift that moved the duel from duty to crime. The epsilon
 *   referent is the standing dueling arrangement itself as this reading sees
 *   it — genuinely coordinated, substantially extractive, peer-enforced —
 *   never the post-erosion world this reading expects. Per the
 *   epsilon-invariance principle this is one of a three-story family:
 *   decline_reading and contraction_reading are separate constraints with
 *   their own epsilon values, linked here via network.affects_constraints.
 *   The claim/metric gap is deliberate: the arrangement is CLAIMED as
 *   tangled_rope (its structural nature while operative) while the end-state
 *   metrics show pitonward drift (theater 0.71, extractiveness 0.36) — the
 *   engine measures that divergence; the claim is not reconciled to the
 *   metrics. KEY AGENTS (by structural relationship): -
 *   regimental_honor_courts: agenda-setter (institutional/identity_locked) —
 *   administers the code, adjudicates honor disputes, enforces participation
 *   - aristocratic_officer_corps: primary beneficiary-and-payer
 *   (organized/identity_locked) — collects cohesion and rank-validation, pays
 *   in officers' blood - hereditary_gentry_status_order: primary beneficiary
 *   (powerful/constrained) — the duel certifies hereditary distinction -
 *   seconds_and_duel_administrators: secondary beneficiary (moderate/mobile)
 *   — runs the machinery, takes standing and fees -
 *   reluctant_challenged_parties: primary target (moderate/trapped) — coerced
 *   to the field - ostracized_refusers: target (moderate/trapped) — ruined
 *   for declining - killed_or_maimed_duelists: ultimate target
 *   (powerless/trapped) — bore the arrangement's terminal cost -
 *   central_state_judicial_apparatus: excluded eroder (institutional/mobile)
 *   — claims grievance adjudication - bourgeois_commercial_classes: excluded
 *   eroder (organized/arbitrage) — built the alternative status currency -
 *   life_insurance_underwriters: excluded eroder (institutional/arbitrage) —
 *   priced duel death as imprudence - duelist_bereaved_families: excluded
 *   voice (moderate/trapped) — no standing to object -
 *   norm_institution_historians: analytical observer (analytical/analytical)
 *   — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.36).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.28).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Erosion Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical sociology/legal history/normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '4c2546fd-9f89-43cb-bce9-1cde573b9da5').
narrative_ontology:cs_kernel_codification('4c2546fd-9f89-43cb-bce9-1cde573b9da5', formalized).
narrative_ontology:cs_authority_grounding('4c2546fd-9f89-43cb-bce9-1cde573b9da5', practice).
narrative_ontology:cs_interpretation_layer_present('4c2546fd-9f89-43cb-bce9-1cde573b9da5').
narrative_ontology:cs_reading_relation('4c2546fd-9f89-43cb-bce9-1cde573b9da5', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c2546fd-9f89-43cb-bce9-1cde573b9da5', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('4c2546fd-9f89-43cb-bce9-1cde573b9da5', foundational, erosion_was_multicausal).
narrative_ontology:cs_axiom_status(erosion_was_multicausal, holdable).
narrative_ontology:cs_axiom_grounding('4c2546fd-9f89-43cb-bce9-1cde573b9da5', erosion_was_multicausal, empirically_contingent).
narrative_ontology:cs_axiom('4c2546fd-9f89-43cb-bce9-1cde573b9da5', secondary, recategorization_partial_and_late).
narrative_ontology:cs_axiom_status(recategorization_partial_and_late, holdable).
narrative_ontology:cs_axiom_grounding('4c2546fd-9f89-43cb-bce9-1cde573b9da5', recategorization_partial_and_late, empirically_contingent).
narrative_ontology:cs_reference_frame('4c2546fd-9f89-43cb-bce9-1cde573b9da5', embedded_multifunction_honor_institution).
narrative_ontology:cs_drift_state('4c2546fd-9f89-43cb-bce9-1cde573b9da5', fin_de_siecle_enforcement_collapse, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4c2546fd-9f89-43cb-bce9-1cde573b9da5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, hereditary_gentry_status_order).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, seconds_and_duel_administrators).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, reluctant_challenged_parties).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, ostracized_refusers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, killed_or_maimed_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, point_of_honor_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, honor_community_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Standing bodies within regiments and gentlemanly clubs that receive complaints of insult, rule on whether honor has been impugned, and press the parties toward a meeting or a negotiated withdrawal of the offending words. Their jurisdiction rests entirely on the honor community's willingness to bring disputes to them rather than to the royal courts; when members stop coming, they have nothing left to administer.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, regimental_honor_courts, agenda_setter,
    institutional, generational, identity_locked, national).

% Commissioned officers across European armies lived under codes making refusal to answer a challenge grounds for dismissal. The code disciplined the corps, sorted promotion among men of equal rank, and marked officers off from civilian life. It also killed and maimed serving officers at a steady rate, and leaving the corps to escape the code meant surrendering career, rank, and the identity that came with them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_corps, payer).

% The titled and landed families whose social precedence the duel certified: a man willing to stake his life on his word demonstrated that his rank rested on something no court could grant or revoke. The families could not simply stop dueling without devaluing the distinction that set them above the commercial classes, yet they watched the practice drain sons and heirs.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, hereditary_gentry_status_order, beneficiary,
    powerful, generational, constrained, continental).

% Friends, regimental colleagues, and hired specialists who carried challenges, negotiated terms, arranged weapons and ground, and attended the field. Their standing rose with successful negotiations — many affairs ended at their hands without shots fired — and their craft of discretion, mediation, and procedure transferred readily to law, diplomacy, and club administration.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, seconds_and_duel_administrators, beneficiary,
    moderate, biographical, mobile, national).

% Gentlemen who received challenges they did not want: men whose only quarrel was a careless phrase at cards, young officers singled out by seniors, duellists facing noted killers. Every available path cost them — the field risked death, a negotiated apology invited accusations of cowardice that followed a man for life, and the courts offered no remedy the honor world would recognize.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, reluctant_challenged_parties, payer,
    moderate, biographical, trapped, national).

% Men who declined to fight and paid the published price: struck from army lists, asked to resign commissions, cut by former messmates, barred from clubs and assemblies. Some rebuilt lives abroad or in commerce; within the honor world their names were finished.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, ostracized_refusers, payer,
    moderate, biographical, trapped, national).

% The men who died on the field or carried wounds for life — the periodic terminal cost the arrangement's credibility required someone to bear. They had accepted the code's terms as their class defined them; the record of their deaths was kept by the same institutions that scheduled the meetings.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, killed_or_maimed_duelists, payer,
    powerless, biographical, trapped, national).

% Royal prosecutors, magistrates, and codifiers who claimed every injury, including insult, as a matter for the sovereign's courts. Edicts criminalized the duel repeatedly from the seventeenth century onward and prosecutions intensified through the nineteenth. The honor community's rule that gentlemen do not litigate kept these bodies outside the settlement of honor disputes for two centuries, while their files quietly accumulated the alternative record.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, central_state_judicial_apparatus, excluded,
    institutional, generational, mobile, national).

% Merchants, manufacturers, and professionals excluded from honor citizenship — a tradesman's challenge would not be answered — who built a parallel currency of creditworthiness, respectability, and printed opinion. Their newspapers ridiculed the duel as aristocratic murder; their sons filled the universities and armies where the old code had to compete with examinations and prudence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_commercial_classes, excluded,
    organized, generational, arbitrage, continental).

% Actuaries and offices that priced lives. Duel deaths entered their tables as preventable mortality: policies excluded deaths by duel, applications asked about challenges given or received, and the growing habit of insuring oneself for one's family attached a money price to the honor world's demand that a man risk it. Their ledgers treated what the code called a point of honor as an imprudence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, life_insurance_underwriters, excluded,
    institutional, generational, arbitrage, continental).

% Widows, parents, and children of men killed in affairs of honor. The code treated a death in the field as closing the matter honorably; families had no standing to reopen it, no forum in which their objection counted, and often a pension or a reputation that depended on calling the death glorious.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, duelist_bereaved_families, excluded,
    moderate, biographical, trapped, national).

% Scholars reconstructing how the honor-satisfaction arrangement worked and why it ended, working from court files, regimental records, insurance registers, press archives, and correspondence. They hold the competing accounts of the ending — attrition, cognitive impossibility, multi-channel dismantling — and the archive lets each be tested against the others.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, norm_institution_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled irreconcilable honor claims between armed gentlemen through a single rule-governed encounter in place of unbounded vendetta: seconds negotiated, rules bounded the violence, a credible stake of life certified sincerity, and a definitive outcome closed the dispute before it spread to kin and clients.
% TRANSFER_FUNCTION: Moved the risk of death or maiming and the duty of compliance onto every member of the honor class subject to challenge, and delivered to the class collectively a positional good — certified rank distinction and a bounded alternative to feud — while diverting dispute-settlement authority from the royal courts to peer-run honor tribunals and seconds.
% ABSENT_VOICES: Civil courts and magistrates, kept out by the rule that gentlemen do not litigate; the commercial classes, excluded from honor citizenship and unable to give or answer challenges; bereaved families, given no standing once a death was ruled honorable; and the killed and maimed themselves. Each sat outside the honor community's jurisdiction by design; their objections survive only in the state's prosecutorial record, the bourgeois press, and probate files.
% DISAPPEARANCE_RATIONALE: Officer promotion cultures, social sorting among gentlemen, and dispute settlement all ran through demonstrated courage-under-challenge and the honor courts' jurisdiction. If the arrangement vanished overnight, gentlemen would route grievances to the courts and the press immediately, status competition would reroute to wealth, office, and display, and the honor tribunals and seconds would lose their caseload — the class's internal order would reorganize around the bourgeois and legal channels already waiting outside.
% FOUNDING_PROBLEM: Bounding private vengeance among an armed hereditary warrior class: before standing courts could reliably adjudicate insults and injuries between nobles, feud cycles threatened both the class's internal order and princely authority, and the ritualized duel bounded the settlement of honor injuries to a single rule-governed encounter.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship on the state's absorption of grievance adjudication in early-modern and nineteenth-century Europe, contemporaneous jurists and anti-dueling campaigners outside the honor class attesting that the courts offered adequate redress for insult, and insurance registers treating duel deaths as ordinary preventable mortality. Honor-class apologia attests the opposite — that the founding problem remained live — which is precisely why corroboration is taken only from outside the benefiting parties.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).
:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state extractiveness is 0.36, not zero: inside closed institutions a refusal still destroyed a career in 1910, but the general grip was broken as courts, press, and prudence normalized exit. Suppression is 0.28 as a RAW structural property — the ostracism machinery and courts-martial pressure that once held the arrangement together had largely lost capacity and legitimacy; per the framework, suppression is unscaled, while the engine scales extractiveness by directionality and scope. Theater is 0.71: by interval end most remaining activity was performative — deliberately bloodless encounters, challenges settled by seconds before shots, the Mensur's scar ritual — form outliving function. Accessibility_collapse is 0.30 because the alternatives the code once suppressed (litigation, public disregard, apology without blood) were restored; resistance is 0.40 because the opposing coalition had largely won and was demobilizing while a rearguard defended the code inside the corps. The suppression_requirement series is authored because this story's subject IS enforcement-capacity change: it traces an inverted U — a mid-century enforcement ratchet (0.60 to 0.76) as defection met harder policing, then collapse (to 0.28) as state prosecution flipped onto the enforcers themselves and recategorization removed the shame that powered peer enforcement. That hump is a single cycle of ratchet-and-collapse, not oscillation, and is not itself an extraction mechanism. All three series run on one shared six-point grid (1780, 1825, 1855, 1880, 1900, 1914) with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the honor courts' position the arrangement was justice itself — their jurisdiction, their procedure, their authority. From the officer corps' position it was constitutive identity: the code made them what they were, and they paid for it in blood without experiencing the payment as imposition. From the challenged party's position the same structure was mortal coercion with no acceptable exit. The excluded eroders saw barbarism awaiting civilization: the state saw injuries belonging to its courts, the bourgeoisie seen an irrational aristocratic cult, the insurers a priced imprudence. Same-nominal-level differentiation is sharpest between officers and bourgeois professionals — comparable standing, opposite exits: the officer's exit was identity_locked (leaving the corps meant ceasing to be himself), the bourgeois's arbitrage-grade (he had already built a status economy that did not need the duel), which is why structurally similar men experienced the arrangement as home versus irrelevance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: the gentry status order and the administrators sit near the beneficiary end; the officer corps is genuinely dual-positioned — beneficiary by declaration, payer by secondary role — and the derivation's low d is defensible because the class net-collected for centuries (it defended the code voluntarily; it could have abandoned it). Victim declarations drive the high-d seats: challenged parties, refusers, and the killed sit near the full-target end, with trapped exit pushing them further. The excluded eroders derive near-symmetric d — outside the arrangement, affected by it without collecting from it. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms produce the correct relationships, and overrides would collide across stakeholders sharing power atoms. On the receipt surface: gain_flow is authored as diffuse affirmatively — each seat was checked, and none captures concentrated rents; the arrangement's yield is a positional good (certified rank, group boundary) consumed pro rata by the honor class, seconds take incidental fees, the courts take instrumental authority, and the extraction's product dissolves into a collectively held order. Fixing cost is prohibitive: the code was peer-enforced, so its own administrators could not abolish it by decree — royal edicts failed for two centuries — and removal only became cheap once the four external channels had recategorized the practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding feud among an armed hereditary warrior class — died with the state's absorption of grievance adjudication, roughly a century before the mechanism itself; what persisted was status maintenance running on inherited forms. The mandate outlived the function, and the R5 mismatch consumer will correctly flag dead-status paired with world_rearranges, cross-checked against the rising theater series. The classification prevents two mislabelings: reading the arrangement as pure extraction ignores the real coordination function (bounded violence replacing vendetta) that made gentlemen defend it; reading it as pure coordination ignores the coerced participants, the ruined refusers, and the dead who paid for the class's positional good. The composite reading adds a third prevention: because erosion ran through four independent channels, no single-edict counterfactual explains the death — which is exactly why the arrangement survived two hundred years of prohibition and then collapsed within three generations of converging pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which account correctly describes how the honor-satisfaction arrangement ended: multiple independent erosive channels plus partial recategorization (this reading), monotonic frequency decline to fringe persistence (decline_reading), or completion of a category-level impossibility (contraction_reading)?',
    'Comparative archival signature tests: continuous frequency curves with stable participant rationale support decline_reading; abrupt semantic discontinuities in honor discourse support contraction_reading; dissociable causal channels each with distinct timing and distinct institutional fingerprints (prosecution files, insurance registers, press campaigns, corps statutes) support this composite reading.',
    'Determines the constraint''s death mode — attrition, cognitive revolution, or multi-channel dismantling — and therefore which sibling stories share this story''s epsilon-referent and which counterfactual interventions the model treats as decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Kernel-level contest among three readings of the honor-satisfaction mechanism''s erosion.').

omega_variable(
    category_shift_independence,
    'Is category-shift an independent fourth erosive channel, or the cumulative downstream effect of the state monopoly, bourgeois norms, and insurance channels?',
    'Trace the appearance of recategorization (duel-as-crime/folly rather than duty) in discourse and institutional records across jurisdictions where only some of the other channels operated; if recategorization tracks the other channels'' presence it is downstream, if it appears autonomously it is a fourth channel.',
    'If downstream, the composite reading reduces to three channels plus epiphenomenon and this story moves structurally nearer decline_reading; if autonomous, the four-channel axiom stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_shift_independence, conceptual, 'Whether recategorization is an independent causal channel or a cumulative effect of the other three.').

omega_variable(
    channel_counterfactual_sufficiency,
    'Would any single channel — state monopoly, bourgeois norms, insurance, or recategorization — have sufficed to erode the arrangement absent the others?',
    'Natural experiments from jurisdictions and stints where one channel dominated: state prosecution without bourgeois ascendance, commercial ascendance under weak states, insurance penetration without criminalization; compare residual dueling rates and enforcement intensity.',
    'Sufficiency of any single channel would reorder the reading''s causal weights and change which counterfactual the model treats as the binding one; joint necessity supports the multicausal axiom as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(channel_counterfactual_sufficiency, empirical, 'Overdetermination versus conjoint necessity among the four erosive channels.').

omega_variable(
    residual_pocket_identity,
    'Are the end-state residuals (German student corps Mensur, late military affairs of honor) the same constraint in decayed form, or a recategorized successor practice — athletic ritual, bonding ceremony — that merely inherits the old forms?',
    'Test whether participants'' governing rationale remains honor-satisfaction (same epsilon-referent) or has shifted to sport and companionship (a new constraint): corps statutes, participant testimony, and the social meaning of the scars as recorded by contemporaries outside the corps.',
    'If the residuals are a successor constraint, the parent mechanism died completely and this story''s end-state extractiveness overstates persistence; if they are the same constraint decayed, decline_reading''s fringe-persistence claim gains ground within this story''s own referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_pocket_identity, conceptual, 'Epsilon-invariance question over the residual practice at interval end.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1780, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_composite_tr_t1780, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1780, 0.15).
narrative_ontology:measurement_basis(hsm_composite_tr_t1780, observed).
narrative_ontology:measurement(hsm_composite_tr_t1825, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1825, 0.22).
narrative_ontology:measurement_basis(hsm_composite_tr_t1825, observed).
narrative_ontology:measurement(hsm_composite_tr_t1855, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1855, 0.34).
narrative_ontology:measurement_basis(hsm_composite_tr_t1855, observed).
narrative_ontology:measurement(hsm_composite_tr_t1880, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1880, 0.48).
narrative_ontology:measurement_basis(hsm_composite_tr_t1880, observed).
narrative_ontology:measurement(hsm_composite_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.62).
narrative_ontology:measurement_basis(hsm_composite_tr_t1900, observed).
narrative_ontology:measurement(hsm_composite_tr_t1914, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1914, 0.71).
narrative_ontology:measurement_basis(hsm_composite_tr_t1914, observed).

% Extraction over time
narrative_ontology:measurement(hsm_composite_be_t1780, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1780, 0.7).
narrative_ontology:measurement_basis(hsm_composite_be_t1780, observed).
narrative_ontology:measurement(hsm_composite_be_t1825, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1825, 0.68).
narrative_ontology:measurement_basis(hsm_composite_be_t1825, observed).
narrative_ontology:measurement(hsm_composite_be_t1855, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1855, 0.64).
narrative_ontology:measurement_basis(hsm_composite_be_t1855, observed).
narrative_ontology:measurement(hsm_composite_be_t1880, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1880, 0.56).
narrative_ontology:measurement_basis(hsm_composite_be_t1880, observed).
narrative_ontology:measurement(hsm_composite_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.44).
narrative_ontology:measurement_basis(hsm_composite_be_t1900, observed).
narrative_ontology:measurement(hsm_composite_be_t1914, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1914, 0.36).
narrative_ontology:measurement_basis(hsm_composite_be_t1914, observed).

% Suppression requirement over time
narrative_ontology:measurement(hsm_composite_su_t1780, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1780, 0.6).
narrative_ontology:measurement_basis(hsm_composite_su_t1780, observed).
narrative_ontology:measurement(hsm_composite_su_t1825, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1825, 0.68).
narrative_ontology:measurement_basis(hsm_composite_su_t1825, observed).
narrative_ontology:measurement(hsm_composite_su_t1855, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1855, 0.76).
narrative_ontology:measurement_basis(hsm_composite_su_t1855, observed).
narrative_ontology:measurement(hsm_composite_su_t1880, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1880, 0.62).
narrative_ontology:measurement_basis(hsm_composite_su_t1880, observed).
narrative_ontology:measurement(hsm_composite_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(hsm_composite_su_t1900, observed).
narrative_ontology:measurement(hsm_composite_su_t1914, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1914, 0.28).
narrative_ontology:measurement_basis(hsm_composite_su_t1914, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the contested kernel honor_satisfaction_mechanism: the colloquial label 'the decline of dueling' conflates three structurally distinct claims about how the arrangement ended. This file instantiates the composite reading (four independent erosive channels plus partial recategorization; end-state epsilon 0.36 with theater 0.71). decline_reading models monotone frequency decay to fringe persistence and would author a longer-lived, steadily binding arrangement; contraction_reading models completed category-level impossibility and would author an arrangement whose violation turned unthinkable, deleting the residuals as misdescribed. Each member carries its own epsilon, beneficiary/victim structure, and drift profile; the edges here join the family so contamination and comparison analyses can traverse it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
