% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Redefined Honor Code Excluding Violence from Legitimate Response (Contraction Reading)
 *   domain: historical sociology / legal anthropology / commitment systems
 *
 * SUMMARY:
 *   Between roughly 1770 and 1900 the normative content of honor itself
 *   changed in the polite societies of Europe and the Atlantic world: the
 *   qualities constituting honorable conduct were redefined around
 *   self-command, restraint, and moral sensibility, and violence ceased to be
 *   a legitimate honor response. Dueling did not merely become costly or
 *   unfashionable — on this reading it became conceptually unavailable: a
 *   gentleman who demanded satisfaction at pistol point was no longer
 *   defending his honor but confessing its absence. This story authors that
 *   claim — the contraction_reading of the honor-violence kernel — as a
 *   single clean, epsilon-invariant constraint: the redefined honor code, the
 *   standing arrangement that governs elite conduct by excluding violence
 *   from honor's repertoire. The epsilon referent is the redefined code
 *   itself, assessed by this reading's own lights, which hold the
 *   redefinition real and causally central. The claim (tangled_rope) and the
 *   metrics are authored independently: the type is what the structure shows
 *   — genuine coordination of non-violent dispute management bound to real
 *   extraction through identity confiscation and recognition gatekeeping —
 *   and the metrics describe the code's actual operation. The sibling
 *   readings are separate constraints with their own epsilon values over
 *   different referents: the drop_reading authors epsilon over the persisting
 *   old code (extraction concentrated on those still bound to it); the
 *   composite_reading authors epsilon over the joint mechanism; this file
 *   authors epsilon only over the redefined code as this reading sees it. The
 *   values differ because the referents differ, not because one observable is
 *   measured two ways. The contest among readings is routed to omega
 *   variables and the network links, not folded into this classification.
 *
 * KEY AGENTS:
 *   - honor_code_adjudicators: agenda-setting seat (institutional/mobile) — club committees, regimental honor boards, courts of honor, and the etiquette codifiers who administer the redefined code and collect its gatekeeping authority
 *   - polite_society_gentry: primary beneficiary (powerful/mobile) — the governed class that gains a non-lethal standing economy and pays its conformity demands
 *   - state_judicial_authorities: secondary beneficiary (institutional/arbitrage) — consolidates the violence monopoly as the rival normative order dissolves
 *   - old_code_officers: primary target (organized/identity_locked) — officers whose professional identity is constituted by the code duello and who experience the redefinition as confiscation of standing
 *   - excluded_honor_claimants: secondary target (moderate/constrained) — men denied recognition (satisfaktionsfähigkeit and its analogues) who bear the code's demands without its protections
 *   - women_of_honor_households: excluded voice (moderate/constrained) — policed as honor's collateral, seated nowhere in the affair of honor
 *   - dishonored_trade_classes: excluded voice (powerless/trapped) — legally incapable of honor, bound to its deference, outside its protections
 *   - historical_sociologists: analytical observer (analytical/analytical) — sees the full structure from outside the recognition economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.38).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.55).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Redefined Honor Code Excluding Violence from Legitimate Response (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical sociology / legal anthropology / commitment systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '3a51d254-bd35-48d4-92ba-6da7cb3e8fad').
narrative_ontology:cs_kernel_codification('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', distributed).
narrative_ontology:cs_authority_grounding('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', practice).
narrative_ontology:cs_interpretation_layer_present('3a51d254-bd35-48d4-92ba-6da7cb3e8fad').
narrative_ontology:cs_reading_relation('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', foundational, honor_redefined_to_exclude_violence).
narrative_ontology:cs_axiom_status(honor_redefined_to_exclude_violence, holdable).
narrative_ontology:cs_axiom_grounding('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', honor_redefined_to_exclude_violence, empirically_contingent).
narrative_ontology:cs_axiom('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', secondary, conceptual_legitimacy_governs_practice).
narrative_ontology:cs_axiom_status(conceptual_legitimacy_governs_practice, holdable).
narrative_ontology:cs_axiom_grounding('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', conceptual_legitimacy_governs_practice, conventional).
narrative_ontology:cs_reference_frame('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', restraint_constituted_honor).
narrative_ontology:cs_drift_state('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', post_world_war_one_honor_dissolution, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3a51d254-bd35-48d4-92ba-6da7cb3e8fad', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, polite_society_gentry).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_judicial_authorities).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, honor_code_adjudicators).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, old_code_officers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, excluded_honor_claimants).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_violence_monopoly_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, civilizing_process_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Club committees, regimental honor boards, courts of honor, and the etiquette writers who codify conduct administer the redefined code: they decide which disputes require formal satisfaction, which apologies restore standing, and who counts as a gentleman at all. The redefinition is their mandate — adjudicating honor without blood is the function that secures their authority — and they move between clubs, regiments, and professions carrying that authority with them. They could widen or narrow the code's demands; what they cannot do is dissolve the recognition economy they sit at the center of without dissolving their own standing.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_code_adjudicators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, honor_code_adjudicators, beneficiary).

% The gentlemen whose disputes the code governs. They gain a standing economy that no longer requires them to risk death or criminal prosecution to defend reputation: insult can be answered with the cut direct, the apology, or the arbiter's ruling. They also pay the code's conformity demands — continuous self-command, polished manners, reputation management — and a gentleman who fails the performance can find his standing quietly withdrawn. Exit is real but costly: retreat to country life or the law courts forfeits the social world the code organizes.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, polite_society_gentry, beneficiary,
    powerful, biographical, mobile, national).

% Courts, prosecutors, and the legal reformers who criminalized dueling. The redefinition does their work for them: when honor itself excludes violence, the state's claim to a monopoly on legitimate force stops competing with a rival normative order. They move freely between prosecuting duelists and tolerating the honor economy's residual rituals, absorbing whichever functions — dispute settlement, reputation adjudication — they choose to take over.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_judicial_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Officers and gentlemen of the old school whose professional identity was constituted by the code duello — the willingness to give satisfaction is what made their word credible and their commission defensible. As the redefinition proceeds, the practices that constituted their standing become evidence of its absence: the duel they were honor-bound to fight is now the act that unmans them. Regimental systems sustained the fusion for generations — in some corps into the twentieth century — because leaving the code meant leaving the identity, and the identity was the career.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, old_code_officers, payer,
    organized, biographical, identity_locked, national).

% Professionals, minorities, and men of uncertain status — in the German lands the Jews and others denied satisfaktionsfähigkeit — who sought admission to the honor economy and were refused recognition by its arbiters. They are bound by the code's demands (deference, conduct, the obligation not to transgress) while being denied its protections: no satisfaction can be demanded of them or given by them, which marks them as standing outside the class the code constitutes. Their money and education buy proximity to gentility but not entry, and the arbiters' refusal is renewed at each petition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, excluded_honor_claimants, payer,
    moderate, biographical, constrained, national).

% The women of the households whose male members duel and whose conduct the honor economy polices as its currency: female reputation is the collateral male honor is contested over, yet women hold no seat in the affair of honor — they cannot give or demand satisfaction, issue or accept challenges, or sit on the boards that adjudicate. The redefinition excluded violence from male honor responses while leaving the sexual double standard of honor largely intact. They would contest the code's terms directly; the conversation has no place set for them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_of_honor_households, excluded,
    moderate, biographical, constrained, national).

% The laboring poor and the legally 'dishonorable' trades — executioners, skinners, and others whose contact with blood or bodies marked them incapable of honor in law and custom. The honor economy is a class monopoly they are legally barred from entering and taxed by in deference: they owe the code's performances without any possibility of its protections. They would object that honor is a property regime dressed as a virtue; no mechanism of the code hears them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dishonored_trade_classes, excluded,
    powerless, generational, trapped, national).

% The analytical seat: comparative-historical researchers of the civilizing process and the dueling economy (the Elias lineage; the Kiernan and Frevert lineages on dueling). They see the whole arrangement — who coordinates, who pays, what is enforced, and the contested causal account — from outside the recognition economy they study.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, honor_code_adjudicators).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem for a class of armed, status-conscious men: how to manage insult and dispute without mutual destruction. The redefined code converts violent self-help into non-violent standing contests — the cut, the apology, the arbiter's ruling — with recognized institutions adjudicating satisfaction, so that reputation can be defended at tolerable cost and disputes terminate.
% TRANSFER_FUNCTION: Moves recognition and standing from those who cannot or will not perform the redefined gentility to those who can; moves deference and gatekeeping authority to the code's arbiters; and moves the dispute-settlement function from private violence to social adjudication and, increasingly, to state law.
% ABSENT_VOICES: Women, whose conduct the honor economy polices as the collateral of male honor, have no seat in the affair of honor. The dishonored trade classes are legally incapable of honor and so outside its protections while bound by its deference demands. Excluded honor claimants (denied satisfaktionsfähigkeit and its analogues) petition for admission and are refused by the very arbiters who adjudicate their exclusion. All three would contest the code's terms; none is in the conversation.
% DISAPPEARANCE_RATIONALE: If the redefined honor code vanished overnight, elite dispute management would reorganize around state law and private settlement immediately; the club and regimental authority structures built on adjudicating honor would lose their mandate; and the gentry's standing economy — which distributes deference through recognized codes of conduct — would have to be rebuilt on some other currency. The arrangement is load-bearing for the social order it governs.
% FOUNDING_PROBLEM: The original code duello solved a problem of a weak state: armed gentlemen needed a credible, rule-governed way to defend reputation against insult when no court would vindicate it and a blow left unanswered was a standing destroyed. The redefined code was built to solve the transition problem: how to keep the standing economy functioning once private violence became intolerable — to the state, to religious conscience, to professionalized elites — without collapsing honor itself.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: evangelical and religious critics of dueling attested at the time that elite private violence was real and intolerable, while denying honor could be its cure; state prosecutors and legal reformers attested the same in the criminalization record; and the modern comparative-historical literature (Elias's civilizing-process studies; Kiernan and Frevert on the dueling economy) documents both the founding problem and its supersession by state law from outside the honor economy. The adjudicating seats themselves attest the problem is live; the external record supports 'superseded but not dissolved,' which is why the status is authored contested rather than dead.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.38 at interval end) because the redefined code's dominant operation is coordination — non-lethal dispute management that demonstrably worked, with elite interpersonal violence falling as it spread — while its extraction is real but bounded: continuous conformity demands on the governed, confiscation of the old code's adherents' standing, and recognition gatekeeping against excluded claimants. Suppression (0.55) is the diffuse coercive machinery of social enforcement — ostracism, club expulsion, regimental career consequence — genuine coercion without state violence; it is both structural and internalized, with the split carried by the suppression_mechanism_ambiguity omega. Theater (0.25) is low-moderate: the code's functions are mostly real, with a ceremonial residue in the face-saving challenge-and-apology rituals that peaked mid-transition, when both codes' demands had to be satisfied at once and the affair of honor became performance. Accessibility collapse (0.45): within the honor framework the violent alternative collapses almost completely once the redefinition is understood — but alternatives to the framework itself (state law, private settlement, studied indifference) persist, so the collapse is partial. Resistance (0.60) is high and sustained: regimental dueling cultures held out for generations past polite-society abandonment (British officers into the 1840s, the German corps into the twentieth century), because the resistance was identity-defending, not preference-defending. Identity-lock dynamics: the fusion is professional — the officer's word and commission were constituted by willingness to give satisfaction — and it broke only when the corps itself was defeated or professionalization decoupled standing from the practice; were the identity frame to break earlier, these payers become mobile, the transition's extraction falls, and the constraint classifies closer to pure coordination. The measurement series run on one shared time grid (1770–1900, six points, all three tracked metrics at every point) so no metric's end-state is backfilled onto earlier times.
 *
 * PERSPECTIVAL GAP:
 *   From the old_code_officers' seat the redefinition operates as confiscation: the same acts that constituted their standing are reclassified as its absence, and their identity lock makes the loss inescapable — from that seat the constraint should compute as something close to pure extraction. From the gentry and state seats it operates as civilizational gain: violence falls, disputes settle, the monopoly consolidates — from those seats it should compute as coordination. The adjudicators' seat is the hinge: they administer the coordination and collect the gatekeeping rents, so the same structure reads as function from the chair and as exclusion from the waiting room. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: state_judicial_authorities (beneficiary with arbitrage-grade exit — it can absorb, tolerate, or prosecute the honor economy at will) sits nearest the beneficiary end; polite_society_gentry (beneficiary, mobile exit) sits near it, damped by the conformity costs its members also pay; honor_code_adjudicators (agenda-setter and beneficiary, mobile) sit low but not lowest — they administer what they collect from. Victim declarations map to high d: old_code_officers (victim, organized, identity_locked) sit near the full-target end — the redefinition is precisely the confiscation of their identity capital, and the lock removes exit; excluded_honor_claimants (victim, moderate, constrained) sit high — the gatekeeping takes from them without their consent and without their exit. The excluded voices (women_of_honor_households, dishonored_trade_classes) are outside the governed set: absent-voice evidence, not derivation inputs. No directionality overrides are authored: the beneficiary/victim declarations plus exit options are expected to derive each seat's d correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The redefinition is conventionally narrated as pure moral progress — a coordination story in which coercion fell away because the concept improved. The tangled_rope classification keeps both halves visible: the coordination is genuine (elite interpersonal violence did fall, dispute management did improve), and the extraction is real (the same structure confiscated the old code's adherents' identity capital and maintained class boundaries through recognition gatekeeping, with the rents accruing to the adjudicating seats). It equally blocks the reverse error of reading the whole honor economy as pure extraction, because the coordination function is documented and the identifiable payers are minorities of the governed set, not the governed class itself. On mandatrophy: the founding problem — defending elite standing against insult — was transformed rather than solved, migrating into defamation law and the press, so the founding_problem_status is authored contested rather than dead; the arrangement persists at interval end because the standing economy it carries is still load-bearing, and the mismatch consumer will find no dead-problem-plus-world-rearranges flag to fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_drop,
    'This constraint is the contraction_reading of the kernel honor_violence_legitimacy. The disagreement with the drop_reading is located in one structural element: whether the normative content of the honor concept itself changed. If the drop_reading is correct — dueling remained structurally legitimate and became practically rare only under external costs (legal penalty regimes, changing warfare, professionalization) — what changes in this constraint''s structure?',
    'Comparative historical analysis of jurisdictions and periods where the documented content of honor is held stable while dueling''s cost structure changed, and the reverse. If dueling frequency tracks cost with honor-content constant, the drop reading carries; if frequency tracks documented changes in honor''s content, the contraction reading carries.',
    'Under the drop reading this constraint''s epsilon referent shifts to the persisting old code: extraction concentrates on those still bound to it, the redefinition is epiphenomenal, and the classification would be re-derived for a constraint whose legitimacy structure never changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_drop, conceptual, 'Committer structure: contraction vs drop reading of the honor-violence kernel; disagreement located in whether honor''s normative content changed.').

omega_variable(
    composite_mechanism_weight,
    'The composite_reading holds that external costs and conceptual redefinition operated simultaneously; what share of dueling''s decline does each mechanism carry?',
    'Quantitative comparative history separating cost-structure variation (legal penalties, firearm lethality, army professionalization, middle-class entry) from documented honor-content variation across jurisdictions and decades.',
    'If external costs carry most of the variance, the redefined code''s consolidation is downstream of cost change rather than its cause; the contraction reading survives only as the mechanism that locked in what costs began, and this constraint''s coordination story shifts toward rational adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_mechanism_weight, empirical, 'Relative weight of conceptual contraction versus external costs in dueling''s decline.').

omega_variable(
    redefinition_vs_reweighting,
    'Was honor''s normative content genuinely redefined (the conceptual space of legitimate honor responses contracted), or was honor always multi-valent and the change a reweighting — courage yielding the center to restraint — with dueling losing the center rather than exiting the set?',
    'Close reading of honor adjudications, etiquette literature, and documented affairs of honor across the transition: did contemporaries treat the violent response as outside honor''s meaning (contraction), or as an honor claim that lost adjudication (reweighting)?',
    'Under reweighting the code''s operation is continuous with the old code — same structure, shifted emphasis — so its costs and coordination would be read as persistent features rather than new ones, and the victim classes would be read as continuous across the transition rather than newly created.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redefinition_vs_reweighting, conceptual, 'CS-framing under-determination: contraction versus reweighting of the honor concept; the obvious framing (the concept changed) versus the less obvious one (the concept''s center of gravity moved).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the redefined code''s suppression structural (regimental career consequence, club expulsion, prosecution exposure) or internalized (the self-command ideal making restraint self-enforcing without external sanction)?',
    'Post-exit suppression trajectory: officers and gentlemen who left the sanctioning institutions — emigration, retirement from regimental systems, the post-1918 corps stripped of their sanctioning power. If honor-conformity persisted without enforcement, the internalized share dominates.',
    'If internalized, the code''s effective suppression exceeds the structural measure and outlives its institutions — explaining the persistence of honor performances after the adjudicating machinery dissolved; the constraint''s persistence profile would read as identity-carried rather than enforcement-carried.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the redefined honor code.').

omega_variable(
    transition_double_bind_attribution,
    'Was the mid-transition extraction peak — old-code adherents paying under the new code while new-code conformists still paid under the old — a designed feature of the redefinition''s enforcement, or an unavoidable cost of any normative transition?',
    'Compare double-bind intensity across institutional settings: civilian clubs (where exit was easier) against regimental systems (where it was not). If the double bind tracks institutional identity-lock rather than the redefinition itself, it is a cost of the lock, not of the code.',
    'If attributable to the redefinition''s enforcement design, the mid-interval extractiveness peak counts against the code''s coordination story; if a generic transition cost, the steady-state profile is the relevant one and the peak is discounted in classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_double_bind_attribution, conceptual, 'Attribution of the transition-era double-bind extraction peak.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1770, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvl_contraction_tr_t1770, honor_violence_legitimacy__contraction_reading, theater_ratio, 1770, 0.15).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1770, observed).
narrative_ontology:measurement(hvl_contraction_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1800, observed).
narrative_ontology:measurement(hvl_contraction_tr_t1830, honor_violence_legitimacy__contraction_reading, theater_ratio, 1830, 0.35).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1830, observed).
narrative_ontology:measurement(hvl_contraction_tr_t1860, honor_violence_legitimacy__contraction_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1860, observed).
narrative_ontology:measurement(hvl_contraction_tr_t1890, honor_violence_legitimacy__contraction_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1890, observed).
narrative_ontology:measurement(hvl_contraction_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement_basis(hvl_contraction_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hvl_contraction_be_t1770, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1770, 0.2).
narrative_ontology:measurement_basis(hvl_contraction_be_t1770, observed).
narrative_ontology:measurement(hvl_contraction_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement_basis(hvl_contraction_be_t1800, observed).
narrative_ontology:measurement(hvl_contraction_be_t1830, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1830, 0.42).
narrative_ontology:measurement_basis(hvl_contraction_be_t1830, observed).
narrative_ontology:measurement(hvl_contraction_be_t1860, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1860, 0.45).
narrative_ontology:measurement_basis(hvl_contraction_be_t1860, observed).
narrative_ontology:measurement(hvl_contraction_be_t1890, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement_basis(hvl_contraction_be_t1890, observed).
narrative_ontology:measurement(hvl_contraction_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(hvl_contraction_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hvl_contraction_su_t1770, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1770, 0.25).
narrative_ontology:measurement_basis(hvl_contraction_su_t1770, observed).
narrative_ontology:measurement(hvl_contraction_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement_basis(hvl_contraction_su_t1800, observed).
narrative_ontology:measurement(hvl_contraction_su_t1830, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1830, 0.6).
narrative_ontology:measurement_basis(hvl_contraction_su_t1830, observed).
narrative_ontology:measurement(hvl_contraction_su_t1860, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1860, 0.62).
narrative_ontology:measurement_basis(hvl_contraction_su_t1860, observed).
narrative_ontology:measurement(hvl_contraction_su_t1890, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement_basis(hvl_contraction_su_t1890, observed).
narrative_ontology:measurement(hvl_contraction_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement_basis(hvl_contraction_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The kernel honor_violence_legitimacy decomposes into three constraint stories because 'why did dueling decline' conflates a legitimacy question (what responses honor admitted), a frequency question (what gentlemen actually did), and a causal question (which mechanism moved it). This file (contraction_reading) authors the legitimacy-change claim with its own epsilon over the redefined code; the drop_reading authors the cost-change claim over the persisting old code; the composite_reading authors the joint claim. Each is a distinct constraint with distinct beneficiaries, victims, and failure modes. They are linked because each is cited as evidence against the others, and the shared upstream empirical record (documented conduct, adjudications, etiquette literature) constrains all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
