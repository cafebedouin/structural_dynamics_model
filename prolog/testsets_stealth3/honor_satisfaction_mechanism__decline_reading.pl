% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor-Satisfaction Mechanism (Code Duello) — Decline Reading
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   Across 1780-1914, the code-governed satisfaction of insults organized
 *   elite conduct in Europe: an affront demanded a challenge, a challenge
 *   demanded seconds, terms, and a field, and refusal carried social or
 *   career annihilation. The practice was illegal nearly everywhere it
 *   flourished, yet juries of gentlemen declined to convict its killers for
 *   generations. Over the interval the arrangement eroded continuously —
 *   prosecution intensified, status markets shifted toward wealth and
 *   profession, religious and press campaigning stripped the code of moral
 *   cover, and affairs migrated toward negotiated, bloodless, ceremonial
 *   forms — until the practice reached fringe status while remaining
 *   conceptually available: challengable, negotiable, practicable in
 *   principle to the end. KEY AGENTS (by structural relationship): the agents
 *   enumerated below are identified by their structural position in the honor
 *   economy, and the same names populate the beneficiary, victim, and
 *   stakeholder surfaces. Epsilon is authored for the standing arrangement
 *   under contest — the dueling practice as it actually operated through the
 *   interval — assessed by this reading's own lights; the reading's endorsed
 *   alternative plays no role in the value.
 *
 * KEY AGENTS:
 *   - code_practitioners_and_seconds: Agenda-setting administrator ([powerful]/[identity_locked]) — writes, interprets, and enforces the code; collects brokerage standing and fees
 *   - landed_gentry_honor_class: Primary beneficiary ([powerful]/[constrained]) — collects the honor economy's status rents: deference, marriage alliances, command authority
 *   - military_officer_corps: Compelled participant and secondary beneficiary ([institutional]/[trapped]) — supplies the code's enforcers and its casualties
 *   - honor_bound_young_gentlemen: Primary target ([moderate]/[trapped]) — bears coerced mortal risk and the social death awaiting refusal
 *   - families_of_the_duel_dead: Residual cost-bearer ([moderate]/[trapped]) — uncompensated bereavement, no seat in any affair
 *   - anti_dueling_campaigners: Excluded voice ([organized]/[mobile]) — no standing inside the code's adjudication for most of the interval
 *   - state_justice_apparatus: External enforcement counterweight ([institutional]/[analytical]) — prosecutes the practice from outside without administering it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.18).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.25).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor-Satisfaction Mechanism (Code Duello) — Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical sociology/legal history/normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '572a3d0d-7578-4f5a-8b95-0af71a2948cf').
narrative_ontology:cs_kernel_codification('572a3d0d-7578-4f5a-8b95-0af71a2948cf', formalized).
narrative_ontology:cs_authority_grounding('572a3d0d-7578-4f5a-8b95-0af71a2948cf', practice).
narrative_ontology:cs_interpretation_layer_present('572a3d0d-7578-4f5a-8b95-0af71a2948cf').
narrative_ontology:cs_reading_relation('572a3d0d-7578-4f5a-8b95-0af71a2948cf', honor_satisfaction_mechanism__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('572a3d0d-7578-4f5a-8b95-0af71a2948cf', honor_satisfaction_mechanism__composite_reading, forecloses).
narrative_ontology:cs_axiom('572a3d0d-7578-4f5a-8b95-0af71a2948cf', foundational, conceptual_availability_retained_through_decline).
narrative_ontology:cs_axiom_status(conceptual_availability_retained_through_decline, holdable).
narrative_ontology:cs_axiom_grounding('572a3d0d-7578-4f5a-8b95-0af71a2948cf', conceptual_availability_retained_through_decline, empirically_contingent).
narrative_ontology:cs_axiom('572a3d0d-7578-4f5a-8b95-0af71a2948cf', foundational, single_mechanism_gradual_enforcement_cost_erosion).
narrative_ontology:cs_axiom_status(single_mechanism_gradual_enforcement_cost_erosion, holdable).
narrative_ontology:cs_axiom_grounding('572a3d0d-7578-4f5a-8b95-0af71a2948cf', single_mechanism_gradual_enforcement_cost_erosion, empirically_contingent).
narrative_ontology:cs_reference_frame('572a3d0d-7578-4f5a-8b95-0af71a2948cf', operative_code_duello_equilibrium).
narrative_ontology:cs_drift_state('572a3d0d-7578-4f5a-8b95-0af71a2948cf', fin_de_siecle_fringe_status, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('572a3d0d-7578-4f5a-8b95-0af71a2948cf', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, landed_gentry_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, code_practitioners_and_seconds).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, honor_bound_young_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_the_duel_dead).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, chivalric_lineage_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, point_of_honor_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write, revise, and administer the published dueling codes; serve as seconds arranging terms, inspecting weapons, and brokering apologies between principals; decide whether a slight demands satisfaction and whether an offered apology suffices. Their standing inside polite society rests on being seen as men who can manage an affair of honor, and brokerage in dangerous negotiations is itself a currency of rank. A second who refuses service or mishandles a negotiation loses the position that constitutes his social identity; stepping away from the trade means ceasing to be, in the eyes of his peers, a man of consequence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, code_practitioners_and_seconds, agenda_setter,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, code_practitioners_and_seconds, beneficiary).

% Own the estates and staff the commissions that the honor economy organizes. Deference, marriage alliances, and command authority flow to families whose men answer challenges and stand behind the code. Maintaining the practice costs them attendance at affairs, exposure of sons to death and prosecution, and the growing ridicule of reformers; leaving the honor economy means accepting diminished standing in the marriage market, the county bench, and the officers' mess — an exit that only became realistic as commercial and professional routes to respectability opened in the latter part of the interval.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, landed_gentry_honor_class, beneficiary,
    powerful, generational, constrained, national).

% Serve under regulations and customs that treat a refused challenge as career-ending; in several armies, regulation or entrenched custom made satisfaction effectively compulsory for commissioned men. Promotion, regimental society, and mess culture all price a man's willingness to fight. The corps also supplies the code's most zealous administrators and draws esprit de corps from the practice, so the same institution that compels the risk also celebrates it. Resigning a commission to escape an affair means financial and social ruin; staying means bearing the risk of death, maiming, or the gallows.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, military_officer_corps, payer,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, military_officer_corps, beneficiary).

% Young men entering society inherit a rule set they did not write: an insult left unanswered closes clubs, dinner tables, and regiments to them. Private diaries and letters record terror, reluctance, and relief when seconds negotiate bloodless outcomes; most did not want to fight. For most of the interval, refusal carries social annihilation, and the fear of that annihilation, not enthusiasm for combat, drives compliance. Only in the final decades does public opinion begin to accept a refused challenge without ruin.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_bound_young_gentlemen, payer,
    moderate, biographical, trapped, national).

% Widows, parents, and children of men killed, maimed, or ruined in affairs receive no remedy: juries drawn from the honor class acquit surviving principals, witnesses forget what they saw, and the code treats the outcome as settled honor rather than compensable injury. They bear the losses permanently and hold no seat in any negotiation that produced them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_the_duel_dead, payer,
    moderate, biographical, trapped, local).

% Evangelical clergy, peace societies, radical journalists, and later physicians campaign against the practice in print, Parliament, and pulpit. Inside the honor community's own adjudication their objections carry no standing — a sermon is not a plea admissible in an affair of honor — and for most of the interval their audience among gentlemen is limited. Their leverage grows with the expansion of the press, the softening of jury attitudes, and the rise of the professional classes their message resonates with.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, anti_dueling_campaigners, excluded,
    organized, generational, mobile, national).

% Statutes declare killing by duel to be murder; judges charge it, prosecutors pursue it, and courts sentence it. For generations, juries of gentlemen refuse to convict, coroners record convenient accidents, and the law's paper severity outruns its practical bite. Over the interval, prosecution intensifies, juries grow willing to convict, and the threat of transportation or hanging raises the price of every affair. The apparatus never runs the practice; it prices it from outside.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_justice_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, landed_gentry_honor_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Where courts could not adjudicate verbal injury among armed gentlemen and vendetta threatened escalating retaliation, the code converted disputes into consensual, rule-governed single combats with negotiated off-ramps — apology, withdrawal, bloodless settlement — thereby pricing insults, making threats credible, making apologies meaningful, and channeling elite violence away from ambush and factional killing.
% TRANSFER_FUNCTION: Moves the risk of death, maiming, and criminal prosecution onto challenged men and their seconds; moves status-capital — deference, marriageability, command credibility — to those who comply with the code; moves brokerage standing and fees to the seconds who administer affairs; and, as the practice decays, hands the adjudication of insults back to courts, newspapers, and the market for public opinion.
% ABSENT_VOICES: Reluctant principals could not speak refusal inside the code without confirming the very cowardice it punished; their objections survive only in private letters and diaries. Widows and the maimed had no seat in any affair. Evangelical and radical critics addressed the public sphere but held no standing within the honor community's own adjudication for most of the interval — that jurisdictional exclusion is precisely what the code's boundary maintained.
% DISAPPEARANCE_RATIONALE: Had the mechanism vanished overnight in 1780, elite dispute resolution would have rearranged immediately — toward vendetta, patronage arbitration, or accelerated state takeover — because honor transactions were organized around it. Having decayed gradually instead, its actual disappearance rearranged nothing: by the fringe stage no standing arrangement depended on it, and that fact is this reading's central evidence. The verdict is authored for the end-state referent the story measures.
% FOUNDING_PROBLEM: In societies lacking an effective state monopoly on violence and lacking courts able to adjudicate reputational injury among armed elites, gentlemen needed a way to deter slander, make commitments credible, and settle disputes without triggering unbounded vendettas. The duel answered with rule-governed, consensual combat that priced insults and channeled elite violence.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by anti-dueling parliamentary testimony, by legal-historical analyses of jury nullification and the state's consolidation of the monopoly on violence, and by the commercial press that ridiculed the code's premises; no surviving honor-community source contends that court-inaccessible reputation adjudication among armed elites remains a live problem.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scalars report the interval's end state (1914) and match the final points of the shared measurement grid. Base extractiveness falls monotonically (0.72 to 0.18) as the reading's two levers bite: state prosecution raised the legal price of every affair, and shifting status markets raised the social price of participating. Suppression_requirement falls in step (0.78 to 0.25): the code's hold on participants was always coercive — ostracism, mess-culture discipline, career ruin — and that machinery attrited as exits opened. Suppression is authored as the constraint's own coercive force on its targets, a raw structural property the engine leaves unscaled; only extractiveness is scaled by directionality and scope. Theater_ratio rises (0.20 to 0.60): as lethal outcomes drew the gallows closer, affairs migrated to negotiated bloodless settlements, ceremonial challenges, and deliberately wide firing — form outliving function, the classic proxy-drift signature. Accessibility_collapse is low at end state (0.22): by 1914 a challenged man could refuse, litigate, publish, or laugh off a slight, and those alternatives were widely understood. Resistance is high (0.78): the practice spent its last century under open attack from pulpits, presses, parliaments, and prosecutors. The claimed type is tangled_rope, authored independently of these metrics: the code genuinely coordinated elite violence — converting vendetta into rule-governed single combat with negotiated exits — while extracting from participants who wanted out, and it required continuous active enforcement at every point in the interval. The late-period profile (high theater, low extraction) shows piton-like symptoms, but the claim stands at tangled_rope because enforcement remained active and targets remained identifiable to the end; the engine computes per-seat types from the structural data and may disagree, and that divergence is the datum. Episodic national revivals (post-1815, Second Empire France) are smoothed in the cross-jurisdictional series and noted here rather than modeled as oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The gentry and seconds seats compute coordination-forward classifications: from inside the honor economy the code is the institution that kept vendetta off the estate and made a gentleman's word bankable. The young-gentleman and officer seats compute extraction-forward classifications: the same rules read as a machine that spends other men's lives to mint status. The officer seat is genuinely dual — the corps administers the code and dies under it — so its computed type should sit between the poles. The families' seat sees only the cost side; the campaigners' seat sees only the injustice; neither participates in the coordination the other seats cite. The justice apparatus experiences the arrangement purely as an object of prosecution, never as a rule it lives under.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry and seconds sit near the beneficiary pole: they collect the status rents and brokerage standing the practice mints, and their costs — attendance, ridicule, sons' risk — are partial and substantially chosen. Young gentlemen and officers sit near the target pole: they supply the bodies and the compliance, their exit is social or career annihilation for most of the interval, and whatever esprit flows back is thin against the mortality. Families sit at the extreme target end with no offsetting flow whatsoever. Campaigners and the justice apparatus are structurally outside the transfer — they neither collect from the practice nor pay into it — so their directionalities stay near neutral while their opposition shifts everyone else's costs. Across the interval the gentry's position drifts toward symmetric: the status rents shrink while ridicule and legal exposure grow, which is the structural signature of the decline this reading describes. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already yield the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating reputation among armed elites where courts could not reach — died as the state consolidated its monopoly on violence and commercial society offered alternative status currencies, but the arrangement persisted for generations on identity and inertia, kept alive by men whose social selves were constituted by the code. Reading the mechanism as pure coordination misses the corpses and the coerced; reading it as pure extraction misses the real vendetta-suppression and commitment services it performed while the founding problem lived. The mandatrophy lens dates the mandate's death to roughly the mid-nineteenth century, when jury-proof prosecution and bourgeois status markets arrived together, and explains the tail: what persisted afterward was maintenance of form by men who could not exit their own identities, not satisfaction of any living need. The flag mandatrophy_resolved is therefore authored true, keyed to the mandate's obsolescence rather than to any metric value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_decline,
    'This constraint is the decline_reading of the honor_satisfaction_mechanism kernel — one persisting practice eroding under enforcement and social cost while remaining conceptually available. Would instantiating contraction_reading (the practice became cognitively unthinkable, a category-level impossibility) or composite_reading (multiple distinct mechanisms operated: state monopoly, bourgeois norms, insurance, category-shift) instead change the structural classification?',
    'Author the sibling stories and compare computed types against the shared evidentiary record: contraction predicts abrupt epsilon collapse with suppressed theater (nothing left to perform once the category dies), composite predicts epsilon dropping in discrete steps as each successor mechanism comes online. Whichever trajectory the corpus reproduces from the same prosecution statistics and duel-count series adjudicates the reading.',
    'Under contraction_reading the terminal type trends toward irrelevance with no parties left to organize; under composite_reading the family splits into separate constraints with independent beneficiary structures. This file''s tangled_rope claim and its smooth declining series are conditional on the decline_reading''s unitary-persistence premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_decline, conceptual, 'Reading-commitment omega: decline versus contraction versus composite accounts of the same kernel.').

omega_variable(
    coordination_function_or_status_cover,
    'Did the code duello genuinely solve an elite-violence coordination problem (vendetta suppression, credible commitment, priced insults), or was the coordination story cover for minting status rents from coerced participants?',
    'Comparative analysis of elite homicide and feud rates in polities with and without codified dueling, controlling for state capacity; archival counts of affairs resolved by negotiated apology without shots fired.',
    'A real coordination effect anchors the tangled_rope claim; a negligible effect would push the computed type toward snare, with the honor economy as extraction wearing procedure as costume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_or_status_cover, empirical, 'Whether the practice''s coordination function was genuine or cover.').

omega_variable(
    internalized_honor_identity_share,
    'How much of the code''s hold on participants was structural sanction (ostracism, career ruin, jury-proof impunity for killing) versus internalized identity (gentlemen who could not conceive of themselves refusing)?',
    'Coded reading of diaries, letters, and trial testimony of reluctant principals across the interval; tracking expressed relief in men excused from affairs versus men who sought them out.',
    'A large internalized share predicts persistence after sanctions lapse — the German student-corps practice outliving every external enforcement structure — raising effective suppression above the structural measure and explaining the fringe tail; a small share makes the decline purely exogenous to enforcement and social cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_honor_identity_share, empirical, 'Structural versus internalized suppression in the honor bond.').

omega_variable(
    enforcement_vs_social_cost_weights,
    'Within this reading''s own causal claim, how much of the decline is attributable to state prosecution (enforcement) versus shifting status markets and religious campaigning (social cost), and were the two levers substitutes or complements?',
    'Cross-jurisdiction natural experiments: duel frequencies in jurisdictions differing in prosecution intensity but similar in commercialization, and vice versa; interrupted time-series around landmark prosecutions and around status-market shocks.',
    'If enforcement dominates, the constraint was held down by the state and could resurge where enforcement lapses; if social cost dominates, the practice was already dead culturally and prosecution merely registered the fact — changing which counterfactual the decline reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_social_cost_weights, empirical, 'Relative causal weight of the reading''s two named levers.').

omega_variable(
    fringe_tail_function_or_inertia,
    'The fringe residue that persisted past the interval''s end (student-corpus duels, occasional political affairs) — did it retain any live function (bonding, scar currency, credibility signaling) or was it pure inertia and performance?',
    'Organizational and ethnographic study of the surviving pockets: whether participants cite functions the wider society no longer prices, and whether the pockets decay once their host institutions modernize or are forcibly remade.',
    'Live residual function means the mechanism never fully died and the decline reading undershoots its own terminal claim; pure inertia confirms terminal atrophy and supports the world_unchanged disappearance verdict authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_tail_function_or_inertia, empirical, 'Status of the post-interval fringe residue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1780, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1780, 0.2).
narrative_ontology:measurement_basis(hono_tr_t1780, observed).
narrative_ontology:measurement(hono_tr_t1815, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1815, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1815, observed).
narrative_ontology:measurement(hono_tr_t1845, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1845, 0.37).
narrative_ontology:measurement_basis(hono_tr_t1845, observed).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1870, 0.46).
narrative_ontology:measurement_basis(hono_tr_t1870, observed).
narrative_ontology:measurement(hono_tr_t1892, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1892, 0.54).
narrative_ontology:measurement_basis(hono_tr_t1892, observed).
narrative_ontology:measurement(hono_tr_t1914, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1914, 0.6).
narrative_ontology:measurement_basis(hono_tr_t1914, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1780, 0.72).
narrative_ontology:measurement_basis(hono_be_t1780, observed).
narrative_ontology:measurement(hono_be_t1815, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1815, 0.63).
narrative_ontology:measurement_basis(hono_be_t1815, observed).
narrative_ontology:measurement(hono_be_t1845, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1845, 0.49).
narrative_ontology:measurement_basis(hono_be_t1845, observed).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1870, 0.37).
narrative_ontology:measurement_basis(hono_be_t1870, observed).
narrative_ontology:measurement(hono_be_t1892, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1892, 0.27).
narrative_ontology:measurement_basis(hono_be_t1892, observed).
narrative_ontology:measurement(hono_be_t1914, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1914, 0.18).
narrative_ontology:measurement_basis(hono_be_t1914, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1780, 0.78).
narrative_ontology:measurement_basis(hono_su_t1780, observed).
narrative_ontology:measurement(hono_su_t1815, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1815, 0.67).
narrative_ontology:measurement_basis(hono_su_t1815, observed).
narrative_ontology:measurement(hono_su_t1845, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1845, 0.53).
narrative_ontology:measurement_basis(hono_su_t1845, observed).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1870, 0.41).
narrative_ontology:measurement_basis(hono_su_t1870, observed).
narrative_ontology:measurement(hono_su_t1892, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1892, 0.32).
narrative_ontology:measurement_basis(hono_su_t1892, observed).
narrative_ontology:measurement(hono_su_t1914, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1914, 0.25).
narrative_ontology:measurement_basis(hono_su_t1914, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' decomposes into three structurally distinct claims about one kernel (honor_satisfaction_mechanism), per the epsilon-invariance principle. This file authors the decline_reading: a unitary practice, declining in frequency through enforcement and social cost, conceptually available throughout, with epsilon falling smoothly. contraction_reading authors the category-death claim (epsilon collapses via cognitive impossibility); composite_reading authors the plural-mechanisms claim (epsilon drops distributed across state monopoly, bourgeois norms, insurance forms, and category shift). Each file carries its own epsilon, beneficiaries, victims, and stakeholders; the files are linked through affects_constraints as one constraint family. This story sits upstream of both siblings: the prosecution statistics and duel-count series authored here are the evidentiary substrate the other two readings reinterpret.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
