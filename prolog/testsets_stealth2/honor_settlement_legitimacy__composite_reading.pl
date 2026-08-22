% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Settlement Legitimacy: Dueling Obligation Regime (Composite Reading)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   Between the codification of the classic dueling codes and the First World
 *   War, the European honor order operated a parallel jurisdiction: written
 *   codes, seconds, courts of honour, and regimental regulation defined which
 *   insults demanded satisfaction and compelled the armed elite to answer
 *   them on the ground. This story instantiates the composite reading of the
 *   honor_settlement_legitimacy kernel: the regime's decline (c. 1700-1914)
 *   was overdetermined - cultural contraction dominant, reinforced by legal
 *   suppression, military professionalization, mass publicity, and the
 *   opening of status-compatible legal venues, each pathway sufficient to
 *   thin the practice and jointly terminal. The claim and the metrics are
 *   authored independently: the claimed type names the regime's structural
 *   nature across its operating life (a genuine settlement coordination
 *   carrying asymmetric, enforced costs), while the scalar metrics snapshot
 *   the terminal phase and the measurement series carries the decline.
 *   Sibling readings (contraction_reading, drop_reading) are separate
 *   constraints linked in the network section; epsilon differences across the
 *   family arise because each reading assesses the same referent under a
 *   different account of what held the regime up and what its residuals were.
 *   KEY AGENTS (by structural relationship): - aristocratic_honor_estate:
 *   Primary beneficiary (powerful/identity_locked) - status monopoly
 *   subsidized by the regime - military_officer_corps:
 *   Beneficiary-administrator (organized/constrained) - enforced the code
 *   internally, collected corporate honor - courts_of_honor_tribunals: Agenda
 *   setter (institutional/constrained) - adjudicated satisfaction obligations
 *   - reluctant_young_officers: Primary target (powerless/trapped) - bore
 *   compelled risk - conscientious_refusing_gentlemen: Target
 *   (moderate/trapped) - socially destroyed for refusal -
 *   bereaved_families_of_duel_dead: Target (powerless/trapped) - bore losses
 *   without redress - fencing_masters_and_seconds: Secondary beneficiary
 *   (moderate/mobile) - fee income; rebranded to sport on exit -
 *   state_judicial_authorities: Inter-institutional actor
 *   (institutional/analytical) - rival jurisdiction, serial prosecution -
 *   church_authorities: Inter-institutional opponent
 *   (institutional/analytical) - conscience leverage outside the honor order
 *   - bourgeois_press_public: Excluded voice (organized/mobile) - the
 *   ridiculing public that shifted legitimacy - historical_analysts:
 *   Analytical observer - sees the full circuit from outside every seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.12).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.08).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Settlement Legitimacy: Dueling Obligation Regime (Composite Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, 'c49f3f34-566c-44dc-9891-bef84108a4f0').
narrative_ontology:cs_kernel_codification('c49f3f34-566c-44dc-9891-bef84108a4f0', formalized).
narrative_ontology:cs_authority_grounding('c49f3f34-566c-44dc-9891-bef84108a4f0', practice).
narrative_ontology:cs_interpretation_layer_present('c49f3f34-566c-44dc-9891-bef84108a4f0').
narrative_ontology:cs_reading_relation('c49f3f34-566c-44dc-9891-bef84108a4f0', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c49f3f34-566c-44dc-9891-bef84108a4f0', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('c49f3f34-566c-44dc-9891-bef84108a4f0', foundational, decline_overdetermination).
narrative_ontology:cs_axiom_status(decline_overdetermination, holdable).
narrative_ontology:cs_axiom_grounding('c49f3f34-566c-44dc-9891-bef84108a4f0', decline_overdetermination, empirically_contingent).
narrative_ontology:cs_axiom('c49f3f34-566c-44dc-9891-bef84108a4f0', foundational, contraction_dominance_with_institutional_reinforcement).
narrative_ontology:cs_axiom_status(contraction_dominance_with_institutional_reinforcement, holdable).
narrative_ontology:cs_axiom_grounding('c49f3f34-566c-44dc-9891-bef84108a4f0', contraction_dominance_with_institutional_reinforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('c49f3f34-566c-44dc-9891-bef84108a4f0', operational_honor_jurisdiction).
narrative_ontology:cs_drift_state('c49f3f34-566c-44dc-9891-bef84108a4f0', post_first_world_war, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c49f3f34-566c-44dc-9891-bef84108a4f0', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocratic_honor_estate).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, fencing_masters_and_seconds).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, reluctant_young_officers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, conscientious_refusing_gentlemen).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, bereaved_families_of_duel_dead).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A transnational estate of titled families and landed gentry whose standing rested on a code of personal honor that only its members could give or receive. The duel economy confirmed rank distinctions, policed marriage alliances and political quarrels, and made refusal to answer an insult impossible without forfeiting place in the estate. Leaving the code was not a priced option: a peer who ignored a challenge ceased, in the eyes of his order, to be a gentleman at all - his name, his family's marriages, and his command of tenants and dependents followed the reputation down.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_honor_estate, beneficiary,
    powerful, generational, identity_locked, continental).

% The commissioned ranks of the European armies adopted the duel as an internal disciplinary instrument: regimental custom and, later, written officer regulations required any officer who was insulted to demand satisfaction and punished those who refused with dismissal or blocked promotion. The corps collected corporate cohesion and a sharp boundary separating officers from civilians and from enlisted men; it also administered the obligation, running courts of honour and court-martial review of refusals. An individual officer could not step outside the practice while remaining in the service.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officer_corps, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, military_officer_corps, agenda_setter).

% Swordmasters, shooting instructors, and the gentlemen who served as seconds earned fees and standing from the traffic in challenges: lessons, weapon supply, transport to the ground, negotiation of terms. When the practice thinned they redirected the same skills to sport fencing, target shooting, and athletic clubs, and the trade survived the custom that had fed it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, fencing_masters_and_seconds, beneficiary,
    moderate, biographical, mobile, regional).

% Regimental boards, club committees, and (late in the period) statutory courts of honour decided which words constituted an insult demanding satisfaction, whether an apology sufficed, and what weapons and odds were fair. They kept the written codes current, absorbed disputes that might otherwise have escalated, and their rulings were the operative law of the practice; their authority ended where the honor order's acceptance ended.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, courts_of_honor_tribunals, agenda_setter,
    institutional, generational, constrained, national).

% Junior officers, subalterns and cadets, absorbed the highest rate of compelled encounters: seniors forwarded insults down the hierarchy, garrison culture treated hesitation as cowardice, and refusal meant resignation, dismissal, or a ruined name in the only profession open to them. Many recorded terror before the ground and relief at wounds that let both sides stop; their mortality and maiming were the practice's direct product.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, reluctant_young_officers, payer,
    powerless, biographical, trapped, national).

% Gentlemen with religious or moral objections - dissenters, devout Catholics, early humanitarians - who declined challenges and paid in full: ostracism from clubs and regiments, caricature in the press as cowards, broken engagements and stalled political careers. Some emigrated; most endured the sentence inside the society that judged them, since the judgment traveled with their name.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, conscientious_refusing_gentlemen, payer,
    moderate, biographical, trapped, national).

% Widows, parents, and children of men killed on the ground. Because juries, regiments, and ministries closed ranks around survivors of duels - treating the killing as honorable or at worst regrettable - families rarely obtained prosecution, pension, or apology; their loss carried no standing in either the honor order or, for generations, the courts.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bereaved_families_of_duel_dead, payer,
    powerless, biographical, trapped, local).

% Ministries, magistrates, and legislatures that banned the duel repeatedly - French edicts from the seventeenth century onward, British statutes and murder charges, Prussian and later imperial officer regulations - while rarely seeing convictions stick. They ran the rival venue: as ordinary litigation became compatible with gentility, their dockets absorbed the disputes the ground had once settled, and their files record the practice's retreat.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_judicial_authorities, observer,
    institutional, generational, analytical, national).

% From the Council of Trent's condemnation forward, pulpit and confessional denounced the duel as sin: refusing absolution to duelists, denying burial to the killed, funding anti-dueling preaching societies in the nineteenth century. They held no seat in the honor order and sought none; their leverage was conscience and the funeral rite.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, church_authorities, observer,
    institutional, civilizational, analytical, continental).

% The commercial classes, journalists, and readers outside the honor law. Newspapers reported every affair of honour to a mass audience that found the ritual absurd rather than admirable; novelists and cartoonists made the punctilious duelist a figure of fun. They had no standing in any court of honour - and their ridicule, once it attached to the practice itself rather than to individual cowards, did more than prosecution to end it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_press_public, excluded,
    organized, biographical, mobile, national).

% Historians and sociologists reconstructing the institution from trial records, regimental archives, memoirs, and the codes themselves; they see the whole circuit - settlement achieved, costs distributed, enforcement machinery, terminal collapse - from outside any seat in it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, aristocratic_honor_estate).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled grave personal disputes among armed elites through a mutually recognized ritual procedure - challenge, terms negotiated by seconds, bounded engagement - where recourse to commoner courts was incompatible with standing and unbounded private vengeance risked feud; it converted potentially endless vendetta into single, terminating encounters and restored nominal parity of honor between parties.
% TRANSFER_FUNCTION: Moved risk of death and maiming, and the compliance costs of constant readiness to answer, from honor-class members generally - concentrated on juniors and the unwilling - upward into the status capital of the established estate; moved fees to fencing masters, surgeons, and seconds; moved deference and publicity to whoever stood well in the code.
% ABSENT_VOICES: The killed and wounded had no voice by definition; their families petitioned for redress and were refused standing by both regiment and jury. Enlisted soldiers lived under officer dueling culture without any say in it. Women were barred entirely from giving or receiving satisfaction, so grievances inside gentle society had no honorable channel at all. The commercial public watched from outside the honor law until the press gave it a rostrum.
% DISAPPEARANCE_RATIONALE: When the practice ended, elite dispute settlement reorganized wholesale around litigation, administrative review, and printed apology; officer promotion and discipline rewired around written evaluation; the fencing trade rebranded as sport; the honor estate's boundary-keeping moved to schools, clubs, and marriage registries. Nothing replaced the duel one-for-one, but every arrangement that had depended on it found a substitute within a generation.
% FOUNDING_PROBLEM: Early modern Europe's armed elites needed a way to settle grave personal quarrels without either submitting to courts associated with commoners and commerce, or letting quarrels run into destabilizing feud among men who wore swords daily.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal historians trace elite dispute migration into courts and administrative channels through nineteenth-century dockets; sociologists of the civilizing process and historians of the duel (Elias, Kiernan, Frevert, McAleer) document the founding problem's obsolescence; regimental archives show courts of honour atrophying as litigation normalized. No defender of the honor order attests that the original problem remains unsolved.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Terminal-phase scalars: extractiveness 0.12 and suppression 0.08 because by 1914 the regime could compel almost no one - refusals drew ridicule rather than ruin, and officer regulations had abandoned compulsory satisfaction. Theater 0.78 because the last encounters were largely staged: prearranged misses, first-blood conventions, seconds settling matters before pistols cleared holsters. Accessibility_collapse 0.15: once the honor framework loosened, alternatives (litigation, printed apology, administrative review, simple disregard) stayed fully open. Resistance 0.55: two centuries of church condemnation, repeated statutes, medical and humanitarian campaigning, and finally mass ridicule. The series runs on one shared seven-point grid so every tracked metric is authored at every examined time point. Extractiveness falls monotonically as the compelled population shrinks; suppression_requirement falls as the enforcement machinery (jury solidarity, regimental compulsion, courts of honour) decayed - an enforcement-decay trajectory, not intensification, which is why suppression_requirement is tracked at all here. Theater rises steeply only in the final half-century: proxy ritual replacing function is Goodhart drift, but critically the spike did not stabilize - the composite reading's contraction edge predicts collapse rather than inertial maintenance, and the record shows exactly that: casualty rates fell, then frequency itself went to zero within a generation of the theater peak.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the estate's chair the regime was constitutive order - the thing that made its members what they were - so its dissolution registers as world-loss, not liberation. From the junior officer's chair the same rules were a death lottery run by superiors. Inside a single regiment both chairs existed simultaneously: seniors collected cohesion and deference while subalterns supplied the risk, which is why the officer corps is authored as beneficiary-administrator rather than a unitary seat. The state and church sat outside the exchange entirely: they saw a rival jurisdiction to dismantle and a sin to preach against, and their experience of the regime (stubborn, unconvictable) reflects enforcement resting on honor-class solidarity rather than on their own instruments.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the estate (identity_locked exit, powerful) sits nearest the beneficiary pole - the regime subsidized its status monopoly; fencing masters collect fees but hold mobile exit, damping their stake; the officer corps collects corporate goods while administering compulsion, placing it low-d but internally heterogeneous. Victim declarations drive the opposite pole: young officers, principled refusers, and bereaved families sit nearest the full-target end, and their trapped exits amplify what the engine computes for them - a junior officer could not resign, refuse, or litigate without losing the only career and name he had. Courts of honour carry mid-to-high directionality: they bore the labor and legitimacy risk of administration while collecting little. Scope amplification matters for the estate: a continental honor order made courage verifiable only through performance before witnesses, thickening the compulsion on every member below its apex. Suppression is authored as a raw structural property and is not scaled; only extractiveness rides directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are prevented. Calling the regime pure predation would erase its real settlement achievement - bounded, terminating dispute resolution among armed men whom ordinary courts could not touch - and would mispredict its end: predation of this scale resists while enforcement holds, whereas the regime dissolved precisely when its legitimating cognition went, coordination value and imposed cost failing together. Calling the terminal phase inertial maintenance would predict indefinite theatrical persistence, which is the drop_reading's implication; the composite reading's contraction edge explains why the theater spike never stabilized into a maintenance equilibrium - performance was the death rattle, not a routine. On the R5 battery the story deliberately trips the mechanical mismatch (founding_problem_status dead crossed with disappearance verdict world_rearranges): the flag's zombie reading is a false positive here - the arrangement is fully dissolved, and the rearrangements the verdict records are completed migrations, not dependencies awaiting a return. Cross-checking the theater path confirms it: theater rose only in the closing decades and terminated with the practice instead of plateauing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_pathway_weighting,
    'Within the honor_settlement_legitimacy kernel, does the composite reading''s causal weighting (cultural contraction dominant, institutional reinforcement secondary) correctly describe the decline, or does a sibling reading''s weighting fit the record better?',
    'Comparative-jurisdiction analysis isolating pathways: regions with early legal suppression but intact honor culture (Prussian officer corps under formal ban) versus late suppression with transformed culture (Britain after the 1840s murder prosecutions); if suppression-only jurisdictions sustained the practice for generations, the institutional pathways were not independently sufficient.',
    'Re-weighting toward the contraction_reading would simplify the account to purely cognitive dissolution; re-weighting toward the drop_reading would recast the terminal phase as persistent residual practice and push the tail toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_pathway_weighting, conceptual, 'Sibling-reading contest over causal weighting within the honor-settlement kernel.').

omega_variable(
    institutional_pathway_sufficiency,
    'Would the material and institutional changes (state prosecution, army professionalization, status-compatible legal venues) have suppressed dueling even absent the cultural transformation — independently sufficient, or merely accelerative?',
    'Natural experiments from transplanted officer cultures retaining honor norms under differing legal regimes: the American South''s persistence under weak suppression, Continental officer corps'' persistence under formal bans; measure practice duration under matched cognition with varying institutions.',
    'If independently sufficient, the composite reading stands as authored; if merely accelerative, contraction approaches monocausality and the extractiveness trajectory was cognition-driven throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_pathway_sufficiency, empirical, 'Whether the reinforcing institutional pathways could have killed the practice alone.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the compulsion on reluctant participants structural (court-martial, cashiering, unemployability) or internalized (shame experienced as unbearable independent of any sanction)?',
    'Post-exit trajectory study: memoirs and correspondence of officers who resigned rather than duel; if shame and social dread persisted after sanctions ceased, the internalized share is substantial.',
    'Internalized compulsion raises the regime''s true hold above the structural measure and strengthens the contraction pathway, since internalized pressure dies only with the honor framework itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of participant compulsion.').

omega_variable(
    terminal_theater_status,
    'Were late-period bloodless encounters (prearranged misses, first-blood conventions, seconds negotiating reconciliation before shots fired) functional residue or already pure performance?',
    'Decade-by-decade casualty and reconciliation-rate series plus seconds'' memoirs on prearrangement; a near-zero casualty rate with high negotiated-outcome rate marks performance.',
    'If performance dominated by 1880, effective decline predates the wars and the drop_reading''s residual-practice frame gains evidential weight; if function persisted, the composite timeline stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(terminal_theater_status, empirical, 'Functional-versus-performative status of the practice''s final half-century.').

omega_variable(
    honor_identity_constitution_depth,
    'How deeply was honor-estate participation identity-constituting versus instrumentally status-protective — did exit fail because members could not conceive of themselves outside the code, or because exit prices were prohibitive?',
    'Diaries, correspondence, and resignation patterns under identical insult conditions across status strata; differential refusal costs by wealth and access to alternative status sources.',
    'Purely instrumental attachment removes the identity-lock reading of estate behavior and lowers derived suppression; deep constitution locks exit and amplifies the burden the regime placed on the estate''s own juniors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_identity_constitution_depth, conceptual, 'Depth of identity fusion in honor-class adherence to the code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1700, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__composite_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1760, honor_settlement_legitimacy__composite_reading, theater_ratio, 1760, 0.14).
narrative_ontology:measurement_basis(hono_tr_t1760, observed).
narrative_ontology:measurement(hono_tr_t1815, honor_settlement_legitimacy__composite_reading, theater_ratio, 1815, 0.2).
narrative_ontology:measurement_basis(hono_tr_t1815, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__composite_reading, theater_ratio, 1850, 0.3).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1880, honor_settlement_legitimacy__composite_reading, theater_ratio, 1880, 0.48).
narrative_ontology:measurement_basis(hono_tr_t1880, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.62).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).
narrative_ontology:measurement(hono_tr_t1914, honor_settlement_legitimacy__composite_reading, theater_ratio, 1914, 0.78).
narrative_ontology:measurement_basis(hono_tr_t1914, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1700, 0.72).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1760, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1760, 0.7).
narrative_ontology:measurement_basis(hono_be_t1760, observed).
narrative_ontology:measurement(hono_be_t1815, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1815, 0.58).
narrative_ontology:measurement_basis(hono_be_t1815, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1880, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1880, 0.32).
narrative_ontology:measurement_basis(hono_be_t1880, observed).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement_basis(hono_be_t1900, observed).
narrative_ontology:measurement(hono_be_t1914, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1914, 0.12).
narrative_ontology:measurement_basis(hono_be_t1914, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1700, 0.75).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1760, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1760, 0.76).
narrative_ontology:measurement_basis(hono_su_t1760, observed).
narrative_ontology:measurement(hono_su_t1815, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1815, 0.6).
narrative_ontology:measurement_basis(hono_su_t1815, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1850, 0.44).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1880, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement_basis(hono_su_t1880, observed).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement_basis(hono_su_t1900, observed).
narrative_ontology:measurement(hono_su_t1914, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1914, 0.08).
narrative_ontology:measurement_basis(hono_su_t1914, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial question 'why did dueling die?' into three epsilon-distinct constraints over one kernel: contraction_reading (epsilon assessed under monocausal cognitive dissolution), drop_reading (epsilon assessed over a residual persistent practice), and this composite_reading (epsilon assessed under multi-pathway convergence with contraction dominance). The referent is fixed across the family - the standing honor-settlement regime - while each reading authors its own epsilon, victim structure, and terminal dynamics. Evidence flow: the composite reading depends on documentation assembled under the contraction reading's research program (framework-transformation records) and bounds the drop reading's residual-persistence claims; edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
