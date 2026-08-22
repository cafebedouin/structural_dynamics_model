% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor-Duel Legitimacy Regime (Drop Reading)
 *   domain: historical sociology / legal anthropology
 *
 * SUMMARY:
 *   Between roughly the Congress of Vienna and the First World War (interval
 *   t0 approximately 1820 to t90 approximately 1914), the European honor duel
 *   presents a puzzle: its practice collapsed unevenly while its legitimacy
 *   barely moved. Statutes criminalized it almost everywhere; juries declined
 *   to convict; regiments punished refusers more readily than shooters. This
 *   story instantiates the DROP READING of the honor-violence-legitimacy
 *   kernel: the honor code's content stayed stable, and what fell was the
 *   frequency of exercising it, driven by externally imposed costs — legal
 *   exposure, army discipline, insurance and pension consequences, the
 *   consolidating state monopoly of violence — rather than by any
 *   redefinition of honor itself. The standing arrangement under contest, and
 *   the referent of every metric below, is the honor-violence-legitimacy
 *   structure as it stood on the eve of the Great War: licensed, thinkable,
 *   rarely fired. The claimed type and the metrics are authored
 *   independently: the claim says tangled_rope; the metrics describe a
 *   structure whose extraction is falling and whose activity is
 *   majority-performative — the divergence is the datum. KEY AGENTS (by
 *   structural relationship): - officer_corps_establishment: agenda-setter
 *   (institutional/identity_locked) — regulates the code through army
 *   regulations and courts of honor; collects cohesion, rank distinction, and
 *   deference-authority from its operation - landed_gentility: primary
 *   beneficiary (powerful/constrained) — draws status and marriage-market
 *   standing from honor credentials; exercises the license rarely by interval
 *   end - junior_officers: primary target (moderate/identity_locked) — bears
 *   the challenge-obligation; exit means resigning the commission that
 *   constitutes professional selfhood - corps_students: secondary target
 *   (powerless/identity_locked) — bound into affair obligations as the price
 *   of corps membership and its social capital - duel_refusers: punished
 *   exit-attempters (powerless/trapped) — those who declined and were
 *   court-martialed, expelled, or socially buried -
 *   duel_widows_and_dependents: collateral bearers (powerless/constrained) —
 *   inherit the deaths; often stranded in a legal gray zone over pensions and
 *   standing to sue - anti_dueling_reformers: excluded voice
 *   (organized/mobile) — clergy, liberal press, leagues; loud publicly,
 *   absent from every adjudicating seat - state_legal_authorities:
 *   inter-institutional actor (institutional/arbitrage) — legislates nominal
 *   prohibition, declines enforcement, supplies the external costs the drop
 *   reading credits - foreign_military_observers: analytical observer
 *   (analytical/analytical) — attaches and commentators comparing national
 *   honor regimes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.48).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.62).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor-Duel Legitimacy Regime (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical sociology / legal anthropology").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '7490c483-03dd-449c-8a29-bbc996959d96').
narrative_ontology:cs_kernel_codification('7490c483-03dd-449c-8a29-bbc996959d96', formalized).
narrative_ontology:cs_authority_grounding('7490c483-03dd-449c-8a29-bbc996959d96', lineage).
narrative_ontology:cs_interpretation_layer_present('7490c483-03dd-449c-8a29-bbc996959d96').
narrative_ontology:cs_reading_relation('7490c483-03dd-449c-8a29-bbc996959d96', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7490c483-03dd-449c-8a29-bbc996959d96', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('7490c483-03dd-449c-8a29-bbc996959d96', foundational, honor_code_content_stability).
narrative_ontology:cs_axiom_status(honor_code_content_stability, holdable).
narrative_ontology:cs_axiom_grounding('7490c483-03dd-449c-8a29-bbc996959d96', honor_code_content_stability, empirically_contingent).
narrative_ontology:cs_axiom('7490c483-03dd-449c-8a29-bbc996959d96', secondary, exercise_price_sufficiency).
narrative_ontology:cs_axiom_status(exercise_price_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('7490c483-03dd-449c-8a29-bbc996959d96', exercise_price_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('7490c483-03dd-449c-8a29-bbc996959d96', licensed_duel_regime).
narrative_ontology:cs_drift_state('7490c483-03dd-449c-8a29-bbc996959d96', eve_of_great_war, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7490c483-03dd-449c-8a29-bbc996959d96', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, officer_corps_establishment).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, landed_gentility).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, junior_officers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, corps_students).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duel_refusers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duel_widows_and_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and amends the army dueling regulations, convenes courts of honor, and rules which affronts demand satisfaction and which refusals count as cowardice. Senior officers sit as judges in affairs brought by juniors, and a junior's career can end by their verdict. The corps draws cohesion, rank clarity, and a shared standard of courage from keeping the code operative, and by the end of the interval it administers a code its members rarely need exercised.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, officer_corps_establishment, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, officer_corps_establishment, beneficiary).

% Families whose standing in county society, clubs, and the marriage market rides on recognized honor credentials. An unanswered slur once meant ruin; by the end of the interval the credential is asserted far more often than defended, and the class collects the distinction while exposing few of its sons.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, landed_gentility, beneficiary,
    powerful, generational, constrained, continental).

% Subalterns and captains bound by regimental convention to answer slurs with a demand for satisfaction. A refusal, unless dressed as illness or negotiated away by seconds, ends a career; accepting risks death or maiming in a pistol or saber encounter. Leaving the regiment would end the obligation, but the commission is the officer's professional self, so the obligation travels with him.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, junior_officers, payer,
    moderate, biographical, identity_locked, national).

% University students in dueling fraternities who owe their fellows a defensive round whenever challenged, and who collect facial scars as visible proof of character. Avoiding the obligation forfeits the corps membership that anchors their social world, and the wounds are suffered young and carried for life.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, corps_students, payer,
    powerless, biographical, identity_locked, regional).

% Officers and gentlemen who declined to fight and paid for it: courts-martial for cowardice, forced resignations, expulsions from regiments and clubs, and social burial that followed them between towns. Some resigned together in groups to blunt the penalty; most faced it alone.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duel_refusers, payer,
    powerless, biographical, trapped, national).

% Wives, children, and parents of men killed in affairs of honor. They inherit the loss and often a legal gray zone: the death was sanctioned by custom yet criminal on paper, which could complicate pensions and leave them without standing to sue seconds or principals.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duel_widows_and_dependents, payer,
    powerless, biographical, constrained, national).

% Clergy, liberal and radical journalists, honor-reform leagues, and physicians who campaign publicly against the duel as superstition and class barbarism. They publish, petition, and testify, but hold no seat in any court of honor, regimental tribunal, or corps congress where the code is actually administered.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, anti_dueling_reformers, excluded,
    organized, generational, mobile, continental).

% Legislatures and ministries that statutorily prohibit dueling while declining to spend enforcement effort on it: prosecutors hesitate, juries acquit, and sentences land lightly, so the statute functions mainly as a price tag on the practice rather than a bar to it. The state can tighten or relax this price at will.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_legal_authorities, observer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, state_legal_authorities, agenda_setter).

% Attachés, travel writers, and comparative-law commentators who document how each nation's honor regime actually behaves — which armies punish refusal, which tolerate the Mensur, which have let the duel die — supplying the cross-national record later analysts rely on.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, foreign_military_observers, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, officer_corps_establishment).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within armed elite strata the code solved a real problem: it bounded private vengeance. Instead of open-ended feud between families and factions, an affront triggered a scripted procedure — challenge, seconds, terms, satisfaction — that converted most quarrels into negotiated settlements and the remainder into single bounded encounters. It also maintained a membership boundary: knowing the code, and being seen to keep it, distinguished the honorable from the crowd.
% TRANSFER_FUNCTION: Moves risk and standing upward: each principal stakes his life or body; refusers surrender careers and belonging; and the verdicts of seconds and courts convert private quarrels into reaffirmed rank. Deference flows from juniors to the seniors who adjudicate; status currency accrues to the class as a whole; the dead and maimed pay in the only coin the code accepts.
% ABSENT_VOICES: The refuser speaks only as a cowardice verdict; the widow inherits the outcome without ever having been party to it; enlisted ranks absorb their officers' honor habits without consultation; and the maimed student's scar is read as character by everyone except, sometimes, the man wearing it. All are outside the rooms where terms are set.
% DISAPPEARANCE_RATIONALE: The officer corps and corps students argue that arrangements hang on the code — promotions, postings, marriages, and club memberships ride on honor standing, and abolishing the obligation overnight would force a rewrite of how affronts are handled and who counts as fit to command. Reformers reply that in Britain the duel already died without visible rearrangement, that courts and police handle what quarrels remain, and that nothing load-bearing is left outside a few garrison towns and university corridors. Both descriptions fit part of the map.
% FOUNDING_PROBLEM: In an age before states reliably monopolized violence, armed elites needed a way to contain the feuds their own honor culture invited: a procedure that let a gentleman vindicate himself without unleashing kin-on-kin warfare, and that gave superiors a handle on private quarrels inside regiments.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, nineteenth-century military reform commissions concluded that civil courts and gendarmerie had absorbed the quarrel-management function the duel once provided; clerical and liberal critics argued the same from pulpit and press; and later historiography of the duel — its feud-containing origins and its displacement by police and courts — documents the dissolution from archives none of the honoring seats controlled. The officer corps itself disputed the finding to the end, which is precisely what makes the corroboration external.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48 at t90) is material but well below its interval-start value (0.74): the obligation structure still binds — German and Austrian officers still fought, the Mensur still drew blood — but the population exposed to coerced participation shrank as external prices rose, and first-blood conventions drained lethality from the encounters that remained. Suppression (0.62) tracks the machinery that makes refusal expensive: courts-martial, corps tribunals, club expulsion; it is authored as a raw structural property and is deliberately not scaled by power or scope. Theater (0.58) crosses the halfway mark in the last third of the interval: challenge choreography ending in reconciliation, scar display, sabrage, and honor rhetoric increasingly substitute for settlement. Accessibility collapse (0.52) reflects half-opened exits — mediated apology, transfer, resignation, the civil lawsuit — alongside the continuing cost of outright repudiation inside the honoring world. Resistance (0.66) is organized and mature: leagues, pulpits, liberal and socialist presses, medical testimony, and quiet mass avoidance. The three tracked metrics share one time grid (t = 0, 15, 30, 45, 60, 75, 90). Suppression_requirement is traced because the story's dynamic is enforcement-capacity change: a ratchet (0.55 to 0.68 by t60) as German and Austrian regulation matured and codified permission-and-punishment matrices, then partial decay (to 0.62) as French-style tolerance spread and official embarrassment grew. Base properties measure the standing arrangement at t90, its end state. No oscillatory cycle drives the series; extraction and theater drift monotonically, and the arc-shaped suppression trace reflects machinery built and then left to rust. The boltzmann declaration is identity_coordination with the conservative default floor: the code's dominant surviving function is boundary maintenance — who counts as honorable — and the corpus warning about identity cover stories applies with a twist, since the identity function is genuinely old and genuinely functional even while carrying the arrangement's coercive freight. Coalition note: the powerless payer seats are not without resource — grouped resignations of refusing officers recur in the record, and each episode temporarily bends the enforcement curve.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the establishment seat the arrangement is an inheritance it administers: a working machine for ranking courage and containing quarrels, experienced as legitimate order. From the junior-officer and corps-student seats the same machine is a tax collected in blood and career, payable because refusal costs more than compliance. The refuser seat experiences pure penalty — the arrangement at its least negotiable. The state seat experiences a nuisance priced rather than prohibited, and its arbitrage (enforce or wink, decade by decade) is precisely the lever the drop reading credits for the fall in practice. Same-role divergence: junior officers and corps students hold the same payer position at adjacent power levels, but their locks differ in content — the officer's lock is a commission that constitutes professional selfhood; the student's is a corps membership that constitutes his entire social world — so formally identical obligations bind with different tightness. The engine computes these divergences from the structural data; nothing here adjudicates them by assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for landed_gentility and the establishment's secondary collection; victim declarations drive high directionality for the four payer seats, amplified by exit texture: identity_locked juniors and students sit nearer the full-target end than any mobile equivalent would, and the trapped refuser sits at the extreme. The widows are an ascribed-target case — they never entered, chose nothing, and still bear the arrangement's costs at full weight. The state's statutes raise everyone's external price while collecting nothing for itself, which is why it is seated as observer-with-agenda rather than beneficiary. Excluded reformers sit outside the directionality economy entirely; their absence is commentary-grade evidence about consensus formation, not a classification input. Scope amplification runs through the transnational honor market: credentials earned in one capital were spent in another, making verification of satisfaction continent-wide and slow. No directionality overrides are authored: the beneficiary/victim-plus-exit derivation captures every seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — containing feud among armed elites under a weak state monopoly — is dead, and the arrangement outlives it, which is why the genealogy interview returns dead and why the receipt surface matters. The classification guards both mislabels. Calling the residue a snare would erase what still works: the majority of affairs of honor ended without shots, because seconds and courts converted quarrels into settlements — a live mediation service the code still performs. Calling it a rope would erase what is captured: deference-authority visibly accrues to the establishment seat, which adjudicates juniors' fates and collects the arrangement's positional gains, and fixing is prohibitive for exactly that seat because abolition would dismantle its own instrument of rank. The honest center is tangled_rope with dated motion: extraction falling, theater past the halfway mark, enforcement rusting at the edges while biting in the strongholds. If the interval extended past 1914, the same series projects a transition toward the theater-heavy, low-extraction, nobody-profits-enough terminal state, and the engine's lifecycle detection — not this commentary — should timestamp it. The accumulation trigger looks for rising extraction and will not fire on a falling series; nothing here manufactures a crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_drop,
    'This file instantiates the drop_reading of kernel honor_violence_legitimacy. Would the sibling readings classify the same history differently — contraction_reading (honor itself redefined until violent vindication became unthinkable) and composite_reading (external-cost drop and semantic contraction operating simultaneously) — and where exactly is the disagreement located?',
    'Comparative authorship of the sibling constraint files; locate the disagreement in the single structural element at issue — whether the honor code''s normative CONTENT changed across the interval or only the PRICE of acting on it.',
    'Contraction predicts epsilon collapse (the victim set dissolves) and disappearance_verdict world_unchanged; composite predicts intermediate epsilon with a dual mechanism; drop predicts persistent moderate epsilon over collapsed frequency. The three profiles are separable in the compiled corpus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_drop, conceptual, 'Committer-frame routing of the three-reading kernel contest.').

omega_variable(
    external_cost_composition,
    'Which external costs drove practice down — legal exposure, army discipline, insurance and pension consequences, urban anonymity, firearm lethality, or the consolidating state monopoly of violence?',
    'Archival decomposition: prosecution and jury-acquittal records, regimental courts-martial logs, conduct-manual revisions, pension-board rulings on dueling deaths.',
    'Recomposes the suppression_requirement arc. Legal-dominance would predict a monotonically rising suppression series; the authored arc peaks mid-interval, so a legal-dominant finding forces revision of the enforcement narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_cost_composition, empirical, 'Composition of the external cost bundle credited by the drop reading.').

omega_variable(
    operative_legitimacy_under_statute,
    'Did operative legitimacy persist beneath nominal prohibition — did juries acquit duelists while regimes punished refusers harder than shooters?',
    'Compare conviction rates in dueling prosecutions against comparable assaults; compare disciplinary outcomes for duelists against outcomes for refusers.',
    'Confirms the drop reading''s core claim (license intact, price external). Failure — statutes actually biting — would push toward the contraction account, with legitimacy eroding semantically all along.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operative_legitimacy_under_statute, empirical, 'The gap between statutory prohibition and operative licensing.').

omega_variable(
    internalized_honor_suppression,
    'Is the measured suppression structural (courts-martial, expulsion, legal exposure) or internalized (identity fusion — a gentleman cannot refuse and remain himself)?',
    'Post-exit trajectories: demobilized and emigrated officers, ex-corps students — does affair-seeking compulsion and scar-display persist once enforcement machinery is out of reach?',
    'An internalized share raises effective suppression above the structural measure and shifts the payer seats toward harder classifications; it feeds the interpersonal suppression-ambiguity protocol directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_honor_suppression, empirical, 'Structural versus internalized mechanism of honor compulsion.').

omega_variable(
    counterfactual_cost_lifting_test,
    'The drop reading uniquely predicts that lifting external costs revives practice. Does the historical record contain such a test?',
    'Natural experiments: Freikorps dueling revivals 1919-1923, French interwar political duels, Weimar-era Mensur resurgence — did frequency rebound wherever enforcement relaxed?',
    'Revival after cost-lifting confirms the drop account; absence of revival favors contraction or composite and would re-date the semantic shift earlier than this reading allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_cost_lifting_test, empirical, 'The drop reading''s distinctive falsifiable prediction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__drop_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__drop_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(hono_tr_t45, honor_violence_legitimacy__drop_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(hono_tr_t75, honor_violence_legitimacy__drop_reading, theater_ratio, 75, 0.53).
narrative_ontology:measurement(hono_tr_t90, honor_violence_legitimacy__drop_reading, theater_ratio, 90, 0.58).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.74).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__drop_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__drop_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(hono_be_t45, honor_violence_legitimacy__drop_reading, base_extractiveness, 45, 0.59).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(hono_be_t75, honor_violence_legitimacy__drop_reading, base_extractiveness, 75, 0.52).
narrative_ontology:measurement(hono_be_t90, honor_violence_legitimacy__drop_reading, base_extractiveness, 90, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__drop_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__drop_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(hono_su_t45, honor_violence_legitimacy__drop_reading, suppression_requirement, 45, 0.66).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__drop_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(hono_su_t75, honor_violence_legitimacy__drop_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement(hono_su_t90, honor_violence_legitimacy__drop_reading, suppression_requirement, 90, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' fails the epsilon-invariance test: measuring the constraint by practice frequency yields one epsilon; measuring it by the honor code's semantic content yields another. Per the decomposition rule these are separate constraints sharing one kernel: drop_reading (this file — persistent moderate extraction over collapsed frequency), contraction_reading (vanishing victim set), and composite_reading (joint causation). Family links run through network.affects_constraints in all three files. The evidentiary gradient runs from the better-archived drop record (prosecution files, courts-martial logs) toward the more interpretive contraction claim, which is why the drop reading sits upstream in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
