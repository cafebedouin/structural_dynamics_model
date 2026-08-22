% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Honor Satisfaction Mechanism (Code Duello) — Composite Erosion Reading
 *   domain: historical sociology / legal history / normative systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism — the code duello together with its
 *   supporting institutions: courts of honor, marshals and seconds,
 *   regimental custom, and the social machinery that destroyed refusers —
 *   governed dispute and status inside the European gentlemanly class from
 *   the early modern period to the eve of the Great War. This file
 *   instantiates the composite reading of that kernel: the arrangement
 *   genuinely coordinated gentlemanly life (it made a gentleman's word
 *   bankable and channeled private vengeance into regulated procedure while
 *   the state was too weak to police either) and it simultaneously consumed
 *   its own members — compliance and mortality from juniors, the challenged,
 *   and refusers, with deference and jurisdiction accruing to the arbiters
 *   who ran it. Its dissolution, on this reading, came through four
 *   independent pressures operating in parallel — consolidation of the
 *   state's violence monopoly, the rise of a bourgeois respectability that
 *   declined to honor the code's currency, actuarial refusal (duel exclusions
 *   in life insurance) that priced the code's dues in money a family could
 *   see, and the recategorization of the duel from satisfaction to murder —
 *   with recategorization necessary but not sufficient. The epsilon referent
 *   is the standing dueling arrangement itself, assessed by this reading's
 *   lights; the sibling readings author their own epsilon over the same
 *   referent in their own files. KEY AGENTS are listed by structural
 *   relationship in key_agents.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - honor_economy_arbiters: agenda-setting administrator (institutional/identity_locked) — runs courts of honor, arranges terms, collects deference and precedence
 *   - - established_gentlemanly_class: primary beneficiary (powerful/constrained) — collects credible-commitment standing across credit, command, and marriage
 *   - - central_state_and_courts: rival jurisdiction and successor (institutional/arbitrage) — subsidized by the code's containment early, dismantles its jurisdiction late
 *   - - challenged_junior_officers: primary target (moderate/trapped) — bears the code's mortality; refusal means ruin
 *   - - duel_refusers: the enforcement machinery's casualties (moderate/constrained) — bear social and professional destruction as public warnings
 *   - - families_of_duelists: cost-bearers with no seat (powerless/trapped) — lose breadwinners and, after insurance exclusions, all compensation
 *   - - bourgeois_respectability_advocates: excluded alternative status economy (organized/mobile) — argues from outside until juries and press force the category open
 *   - - life_insurance_offices: excluded actuarial seat (institutional/arbitrage) — re-prices the code's risk by refusing to underwrite it
 *   - - historical_sociology_observer: analytical seat (analytical/analytical) — compares polity timing across the four mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.28).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.52).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Code Duello) — Composite Erosion Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical sociology / legal history / normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '633c9e91-c459-4e59-9706-aa7a84c95d5c').
narrative_ontology:cs_kernel_codification('633c9e91-c459-4e59-9706-aa7a84c95d5c', formalized).
narrative_ontology:cs_authority_grounding('633c9e91-c459-4e59-9706-aa7a84c95d5c', practice).
narrative_ontology:cs_interpretation_layer_present('633c9e91-c459-4e59-9706-aa7a84c95d5c').
narrative_ontology:cs_reading_relation('633c9e91-c459-4e59-9706-aa7a84c95d5c', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('633c9e91-c459-4e59-9706-aa7a84c95d5c', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('633c9e91-c459-4e59-9706-aa7a84c95d5c', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('633c9e91-c459-4e59-9706-aa7a84c95d5c', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('633c9e91-c459-4e59-9706-aa7a84c95d5c', secondary, recategorization_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(recategorization_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('633c9e91-c459-4e59-9706-aa7a84c95d5c', recategorization_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('633c9e91-c459-4e59-9706-aa7a84c95d5c', code_duello_parallel_jurisdiction).
narrative_ontology:cs_drift_state('633c9e91-c459-4e59-9706-aa7a84c95d5c', pre_great_war_europe, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('633c9e91-c459-4e59-9706-aa7a84c95d5c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, honor_economy_arbiters).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, established_gentlemanly_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, central_state_and_courts).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, challenged_junior_officers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, duel_refusers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior officers, marshals of the lists, and courts-of-honor members who arrange challenges, set weapon and distance terms, adjudicate precedence disputes, and decide what counts as an affront demanding satisfaction. Their standing inside the class flows from this administrative role; renouncing it would mean renouncing the rank, lineage, and deference their lives are built on. They adjudicate and arrange far more often than they fight.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_economy_arbiters, agenda_setter,
    institutional, generational, identity_locked, continental).

% The landed and office-holding elite whose word, credit, and political alliances are made credible by the shared willingness of class members to risk their lives over affronts. They pay dues in occasional risk but collect standing: a gentleman's promise is bankable, his marriageable, his command obeyed. Drifting into bourgeois respectability is possible but costs standing in regiment, club, and society, and most who drift do so gradually and at a discount.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, established_gentlemanly_class, beneficiary,
    powerful, generational, constrained, continental).

% Legislatures and courts that formally prohibited dueling for over a century while gentlemanly juries and officer courts declined to convict. As administrative capacity grew, the state absorbed the disputes the code used to govern — defamation actions, assault prosecutions, police suppression — and claimed the violence jurisdiction the code had held. It free-rode on the code's containment of private vengeance while that containment was cheaper than policing, then dismantled the code's jurisdiction once its own courts could do the work.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, central_state_and_courts, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, central_state_and_courts, beneficiary).

% Subalterns and junior gentlemen bound by regimental custom: refusing a challenge brings dismissal and social ruin, accepting it brings a real chance of death. They fight disproportionately often relative to the challenges they issue, and their compliance is what the code's daily operation runs on. There is no third door open to them inside the regiment.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, challenged_junior_officers, payer,
    moderate, biographical, trapped, national).

% Gentlemen who declined satisfaction on principle — evangelical conviction, Quaker testimony, prudence, or disbelief in the code. They are dismissed from regiments, blackballed from clubs, cut in society, and denied advancement; their ruined careers serve as the code's public warnings. A few with independent fortunes or dissenting networks survive the ruin; most do not.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, duel_refusers, payer,
    moderate, biographical, constrained, national).

% Widows and children of men killed under the code. They hold no seat in any court of honor, receive no say in whether a challenge is accepted, and bear the loss outright. From the 1820s onward, life offices' duel exclusions mean even the financial compensation available to other widows is contractually denied to them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, families_of_duelists, payer,
    powerless, biographical, trapped, national).

% Evangelical movements, commercial and professional classes, and reformist press who hold that killing for satisfaction is murder and that prudence outranks punctilio. The honor economy does not recognize their standing — gentlemen give no satisfaction to tradesmen — so they argue from outside: sermons, newspapers, associations, and eventually juries that convict. They built and inhabit an alternative status economy that does not need the code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_respectability_advocates, excluded,
    organized, generational, mobile, continental).

% Life offices that, from the 1820s, inserted clauses excluding death by duel from coverage. They never sought a seat in any court of honor; their actuarial refusal made the code's risk financially legible — a gentleman's death in a duel left his family nothing, pricing the code's dues at a level families could see. Their exclusions stand or fall with underwriting judgment alone.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, life_insurance_offices, excluded,
    institutional, generational, arbitrage, national).

% The analytical seat comparing polity-by-polity timing: Britain's early bourgeois turn and last fatal duel in 1852, France's persistence to 1914, Germany's Mensur surviving as a ritualized non-lethal fencing custom inside student corps. Takes testimony from all seats and the archival record; holds no stake in the code's standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, historical_sociology_observer, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, honor_economy_arbiters).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of an armed, self-governing elite in an era of weak central enforcement: a class whose members will risk their lives over their word can extend credit, command, and alliance without external guarantee. Secondarily it channels private vengeance into regulated procedure — seconds, terms, a chance of death rather than a feud — containing violence the period state could not police.
% TRANSFER_FUNCTION: Moves compliance and mortality risk from the broad gentlemanly class — disproportionately from juniors, the challenged, and refusers — into the maintenance of a status economy whose deference, precedence, and jurisdiction over gentlemanly violence accrue to the code's arbiters and the established elite; and moves adjudication of honor disputes out of state courts into the honor economy's own procedures.
% ABSENT_VOICES: Families of the killed had no seat in any court of honor; refusers were heard only through their ruin; evangelical, commercial, and actuarial voices stood wholly outside the code's recognition — a court of honor could not receive an insurance schedule or a sermon as evidence, and gentlemen gave no satisfaction to tradesmen. These seats enter the record only when juries, legislatures, and underwriters force the code's category open from without.
% DISAPPEARANCE_RATIONALE: The mechanism's actual dissolution rearranged the elite world along exactly the seams it had organized: honor disputes moved into defamation and assault courts; gentlemanly credit re-founded on commercial instruments, references, and incorporation; regimental discipline re-founded on written regulation; the surviving corps rituals recategorized themselves as sport rather than satisfaction. Nothing reverted to a natural baseline — every replacement had to be built, which is why the dissolution took the full interval and ran through four independent mechanisms rather than one.
% FOUNDING_PROBLEM: How does an armed, self-governing elite maintain credible mutual commitments and contain private vengeance before a central state can enforce either? The code of honor answered with a private legal order: satisfaction by duel, administered by the class itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by three seats: state judicial records (defamation and assault dockets absorbing the code's caseload as the monopoly consolidated), commercial credit institutions (lending to gentlemen re-founded on instruments, references, and collateral rather than blood-backed word), and the post-reform officer corps itself, which did not collapse when regimental dueling was suppressed — internal evidence that the function had been superseded, offered by the very class the code governed. The code's own arbiters attested the founding problem as still live deep into the nineteenth century; that attestation, coming only from inside the beneficiary set, is precisely the cover-story signal the corroboration rule exists to catch.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction declines monotonically (0.58 to 0.28) as the code's coordination function is absorbed by successors — courts take defamation, commercial law takes credit, written regulation takes regimental discipline — and its reach collapses to enclaves. Suppression follows an inverted U: it rises from 0.55 to 0.76 as exits open (bourgeois respectability, insurance, refuser networks), because a code that once held by identity and automatic exclusion must ratchet up active enforcement — regimental compulsion, blackballing, social death — against members who now have somewhere to go; it then falls to 0.52 as the category shift delegitimizes the enforcement machinery itself (juries convict, regiments stop backing challenges, corps retreat into enclaves). Theater rises throughout (0.14 to 0.58): as function atrophied, surviving practice grew performative — bloodless French duels, the Mensur's scar ritual — which is the recategorization made visible. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. The code's suppression was jointly structural (regimental compulsion, economic dependence, social-death enforcement) and internalized (honor fused with self); the omega suppression_internalized_share carries the split, and the Mensur's persistence after structural enforcement lapsed is the natural experiment that isolates the internalized share. accessibility_collapse is 0.30 at interval end: the general alternative space has re-opened, though enclaves retain local collapse — a corps member who refuses the Mensur still faces expulsion — and that enclave asymmetry is itself evidence for the composite account: the code survived only where exit stayed closed. resistance 0.50: the residue meets real, active resistance (prosecution, university bans, church condemnation) while its holders defend it. fixing_cost is prohibitive: at no point in the interval could any single seat fix the arrangement at a cost below its benefit to that seat — the state could not enforce against its own officer corps and juries; the class could not exit collectively without dissolving the status economy its standing rested on; refusers fixed it only for themselves, at ruinous personal price. Removal became possible only as the four mechanisms matured in parallel, each lowering the fix-cost for the others, which is the composite reading's central claim. The claim is tangled_rope, authored from the reading's structural assessment of the mechanism as such — genuine coordination carrying asymmetric extraction, held by active enforcement; the end-state metrics are eroded, and a computed terminal type diverging from the claim (a piton-flavored residue) would corroborate the recategorization story rather than contradict it. All series run on one shared six-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same code. From the arbiter seat the arrangement is the guarantee of a civilization of word-keeping: they administer it, rarely fight in proportion to their adjudications, and experience its erosion as barbarism. From the subaltern seat it is a death-trap with no third door — refusal is ruin, acceptance is mortality — and the same erosion reads as emancipation. The refuser seat experiences the enforcement machinery directly and at full cost; the family seat bears the code's outcomes with no seat in any court that arranged them. The state seat is split across time: for a century it free-rode on the code's containment of private violence, then, as its courts and police matured, it dismantled the code's jurisdiction as a rival — the same institution experiences the constraint as subsidy early and as usurpation late. The insurance seat never entered the honor economy at all; its actuarial refusal re-priced the code from outside, which no insider seat could do. Identity-lock dynamics: the arbiters' lock is institutional-relational — honor is not a possession but a constitutive relation, and to renounce the code is to unmake the self that rank, lineage, and office built; that frame did break for the class at large (the refusers and the bourgeois turn broke it), and the break is visible in the suppression decay after the interval midpoint. Coalition note: the powerless family seat could not act alone and did not — its leverage arrived through alliance with the organized excluded seat (reformist press, evangelical networks, insurance litigation), not through its own position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the arbiters (d near the beneficiary end — they collect deference, precedence, and jurisdiction from the code's operation), the established class (low d — net collectors of credible-commitment standing across credit, command, and marriage, paying dues in occasional risk), and the state (derived low d from its beneficiary declaration — correct for the operation era, when the code's containment subsidized a state too weak to police elite violence; the later dismantling era is carried by the measurements and narrative rather than by an override, since the schema's override keying cannot separate the state from the other institutional seats without distorting them). Targets: junior officers (high d — trapped, bearing mortality), refusers (high d — they pay the enforcement's full price), and families (highest d — powerless, no exit, no seat, and after the insurance exclusions not even compensation). One override: bourgeois_respectability_advocates at organized/0.55 — the derivation cannot see their position because they are declared neither beneficiary nor victim, yet the code's recognition economy actively devalued their status order (gentlemen gave no satisfaction to tradesmen; bourgeois honor was not a currency the courts of honor recognized), making them mild structural targets of the code's devaluation rather than neutral outsiders. Life insurance offices are left to the canonical fallback: they neither collect from nor pay into the code; their relation is refusal to underwrite it. Receipt: the extraction's gains land on the arbiter seat — deference, precedence, and jurisdiction are what the extracted compliance purchases — so gain_flow names honor_economy_arbiters; the class seat benefits from the coordination good but does not capture the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible commitment and vengeance containment for an armed elite before a capable central state — is dead: courts absorbed the disputes, commercial instruments replaced blood-backed word, and written regulation replaced regimental custom. The mandate outlived its function only in residue (the Mensur's scar ritual), which is why founding_problem_status is dead while disappearance_verdict is world_rearranges: the world the code organized did rearrange, and rearranging it took successor institutions a century and a half to build. The pairing (dead founding problem on a world that rearranges) is honest and is corroborated by the rising theater path of the residue. The composite classification prevents two opposite mislabelings. Read as a mountain, the code's longevity would look like natural law and its erosion like inevitability — but nothing here was natural: successor institutions had to be constructed, polities diverged (Britain done by 1852, France persisting to 1914, Germany recategorizing rather than abandoning), and the arrangement's defenders fought for every inch. Read as a pure snare, the code's coordination function disappears, and with it the explanation for why gentlemen who despised dueling still submitted for two centuries and why the practice died so unevenly across polities — a pure-extraction account cannot explain the credible-commitment work the code demonstrably did. Tangled rope holds both: real coordination, real extraction, and a dissolution that required four independent pressures because no single lever could release a structure that was doing genuine work for its administrators while consuming its juniors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_mechanism_plurality,
    'Is the dissolution of the honor satisfaction mechanism one composite process of independent mechanisms plus recategorization (this reading), or is it dominated by a single mechanism — cognitive category-collapse (contraction_reading) or frequency-decline to fringe status (decline_reading)?',
    'Comparative polity analysis with mechanism ablation: identify polities and periods where each mechanism was present or absent and test which combinations predict the code''s hold and release.',
    'If a single mechanism dominates, this reading collapses toward that sibling''s constraint and the epsilon trajectory should be re-authored on the sibling''s terms; if the mechanisms were jointly necessary, the composite reading stands and the siblings'' classifications over-attribute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_index_mechanism_plurality, conceptual, 'Whether erosion was multi-mechanism (this reading) or single-mechanism (siblings).').

omega_variable(
    mechanism_independence_question,
    'Were the four mechanisms — state violence monopoly, bourgeois norms, insurance exclusion, category-shift — genuinely independent pressures, or downstream expressions of one driver such as state administrative capacity?',
    'Timing and sequencing analysis across polities: France had a strong state yet dueling persisted to 1914; Britain had weak officer compulsion, strong bourgeois norms, and insurance, and dueling died by 1852; Germany''s strong state coexisted with the Mensur''s survival in recategorized form.',
    'If the mechanisms share a single driver, the composite reading reduces to a one-mechanism story and the independence claim in its foundational axiom fails, with classification consequences running through that axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_question, empirical, 'Independence of the four erosion mechanisms.').

omega_variable(
    class_net_benefit_ambiguity,
    'Did the established gentlemanly class net-benefit from the code, or did the code consume the class as a whole with gains concentrated at the arbiter seat?',
    'Rank-distributed mortality and challenge data: who issued challenges, who fought, who died, by rank and generation, from regimental records and duel memoirs.',
    'If the class as a whole net-paid, the class seat''s directionality rises toward symmetric and the coordination-function claim narrows to the arbiter seat alone, moving the arrangement''s computed classification toward pure extraction administered by its insiders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_net_benefit_ambiguity, empirical, 'Whether the class seat is net beneficiary or net payer.').

omega_variable(
    recategorization_necessity_test,
    'Was the category shift — the duel recast as murder rather than satisfaction — necessary for final dissolution, or would state monopoly, bourgeois norms, and insurance have sufficed without it?',
    'The French test case: by 1880 all three non-categorical pressures were strong, yet lethal dueling persisted among officers and politicians until prosecution and press recategorization completed; compare with jurisdictions where recategorization came earlier.',
    'If recategorization was the binding mechanism, the composite reading converges causally toward contraction_reading while retaining mechanism plurality; if it was redundant, the reading''s secondary axiom fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recategorization_necessity_test, empirical, 'Necessity of the category shift for final dissolution.').

omega_variable(
    suppression_internalized_share,
    'Was the code''s hold on its members primarily internalized (honor fused with self, refusal unthinkable) or structural (regimental compulsion, economic dependence, social-death enforcement)?',
    'Defector trajectory comparison: men with independent fortunes, dissenting religious networks, or civilian professions exited with less ruin than regiment-bound officers; the residual Mensur''s persistence inside corps — where structural compulsion lapsed but identity held — isolates the internalized share.',
    'A high internalized share explains the code''s persistence after structural enforcement decayed and predicts slow residual decay even under hostile law; a low share predicts the residue collapses once regimental and corps compulsion is legally removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_share, empirical, 'Structural versus internalized share of the code''s suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_composite_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(hsm_composite_tr_t32, honor_satisfaction_mechanism__composite_reading, theater_ratio, 32, 0.17).
narrative_ontology:measurement(hsm_composite_tr_t64, honor_satisfaction_mechanism__composite_reading, theater_ratio, 64, 0.23).
narrative_ontology:measurement(hsm_composite_tr_t96, honor_satisfaction_mechanism__composite_reading, theater_ratio, 96, 0.32).
narrative_ontology:measurement(hsm_composite_tr_t128, honor_satisfaction_mechanism__composite_reading, theater_ratio, 128, 0.45).
narrative_ontology:measurement(hsm_composite_tr_t160, honor_satisfaction_mechanism__composite_reading, theater_ratio, 160, 0.58).

% Extraction over time
narrative_ontology:measurement(hsm_composite_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(hsm_composite_be_t32, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(hsm_composite_be_t64, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 64, 0.52).
narrative_ontology:measurement(hsm_composite_be_t96, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 96, 0.44).
narrative_ontology:measurement(hsm_composite_be_t128, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 128, 0.35).
narrative_ontology:measurement(hsm_composite_be_t160, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 160, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hsm_composite_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hsm_composite_su_t32, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(hsm_composite_su_t64, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 64, 0.7).
narrative_ontology:measurement(hsm_composite_su_t96, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 96, 0.76).
narrative_ontology:measurement(hsm_composite_su_t128, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 128, 0.64).
narrative_ontology:measurement(hsm_composite_su_t160, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 160, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel decomposes into three readings over one shared referent — the code duello as the standing arrangement governing gentlemanly disputes. This composite reading authors epsilon for that arrangement as a functioning coordination structure carrying asymmetric extraction, eroding through four independent pressures plus recategorization. decline_reading authors the same referent as a practice persisting at declining frequency; contraction_reading authors it as a category that became cognitively impossible. The epsilon values differ because the readings locate different structures in the same historical record; per the epsilon-invariance rule they are separate files linked here rather than one story with a measurement parameter. The relation structure: this reading influences decline_reading (its mechanism-plurality account changes what the frequency record means — a jurisdiction dissolving, not a practice fading — without ruling out the frequency observation) and coexists_with contraction_reading (rival causal decompositions of the endgame, both live historiographic positions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
