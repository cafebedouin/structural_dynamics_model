% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Decline via Overdetermined Causal Mechanisms (Composite)
 *   domain: legal/cultural/historical
 *
 * SUMMARY:
 *   In 1750, dueling was a socially legitimized dispute-resolution mechanism
 *   among American gentry and officers; by 1920, it was culturally
 *   unthinkable and legally prohibited. Historians have identified four
 *   independent sufficient causes for this disappearance: (1) explicit legal
 *   prohibition enacted by states and enforced through prosecution; (2)
 *   institutional substitution, wherein courts and legal practice absorbed
 *   the dispute-resolution function dueling performed; (3) cultural shift
 *   from honor-culture to dignity-culture norms, delegitimizing
 *   honor-satisfaction as a rationale for violence; (4) Civil War trauma and
 *   post-1865 reconstruction, which delegitimized honor-driven violence as
 *   antebellum barbarism and subordinated regional honor cultures to federal
 *   institutional order. The overdetermined reading asserts that NO SINGLE
 *   mechanism was necessary—each was sufficient in isolation—yet ALL FOUR
 *   operated simultaneously, making dueling's decline causally
 *   overdetermined. This means the constraint itself (the combined
 *   suppressive machinery) cannot be decomposed into single mechanisms with
 *   separable ε values: the kernel is the unavoidable confluence of these
 *   pathways, not any one pathway alone.
 *
 * KEY AGENTS:
 *   - State apparatus: Enacts and enforces anti-dueling statutes; monopolizes legitimate violence.
 *   - Legal profession: Accumulates power and fees as dispute resolution institutionalizes; advocates for statutory prohibition.
 *   - Industrial bourgeoisie: Benefits from predictable legal dispute-resolution; politically supports anti-dueling measures.
 *   - Post-Civil War institutional order: Treats dueling as antebellum relic; uses reconstruction authority to suppress it.
 *   - Honor-culture practitioners: Identity-locked to dueling norm; face legal prosecution and institutional exclusion.
 *   - Southern gentry class: Regionally invested in honor-culture; constrained by military defeat and reconstruction subordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.62).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Decline via Overdetermined Causal Mechanisms (Composite)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "legal/cultural/historical").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e3237cee-7c8d-4791-9b19-7ed49fc7c364').
narrative_ontology:cs_kernel_codification('e3237cee-7c8d-4791-9b19-7ed49fc7c364', distributed).
narrative_ontology:cs_authority_grounding('e3237cee-7c8d-4791-9b19-7ed49fc7c364', extraction).
narrative_ontology:cs_interpretation_layer_present('e3237cee-7c8d-4791-9b19-7ed49fc7c364').
narrative_ontology:cs_reading_relation('e3237cee-7c8d-4791-9b19-7ed49fc7c364', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3237cee-7c8d-4791-9b19-7ed49fc7c364', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('e3237cee-7c8d-4791-9b19-7ed49fc7c364', foundational, causal_overdetermination_principle).
narrative_ontology:cs_axiom_status(causal_overdetermination_principle, holdable).
narrative_ontology:cs_axiom_grounding('e3237cee-7c8d-4791-9b19-7ed49fc7c364', causal_overdetermination_principle, empirically_contingent).
narrative_ontology:cs_axiom('e3237cee-7c8d-4791-9b19-7ed49fc7c364', foundational, mechanism_independence_thesis).
narrative_ontology:cs_axiom_status(mechanism_independence_thesis, holdable).
narrative_ontology:cs_axiom_grounding('e3237cee-7c8d-4791-9b19-7ed49fc7c364', mechanism_independence_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('e3237cee-7c8d-4791-9b19-7ed49fc7c364', honor_culture_dispute_resolution_regime).
narrative_ontology:cs_drift_state('e3237cee-7c8d-4791-9b19-7ed49fc7c364', post_civil_war_modernization_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e3237cee-7c8d-4791-9b19-7ed49fc7c364', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_profession).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, industrial_bourgeoisie).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_american_institutional_order).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_gentry_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_institutional_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts anti-dueling statutes starting in the early 1800s, prosecutes duelists, and uses state authority to enforce the cultural transition away from honor-based violence. The state benefits by monopolizing the legitimate use of force and eliminating a competing claim-settlement mechanism. Enforcement is uneven: active prosecution in some jurisdictions and eras, institutional pressure and social ostracism in others. The state's institutional position means it bears no exit cost—anti-dueling enforcement is part of its modernization project.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Accumulates professional authority and revenue as courts and legal procedure become the institutionalized path for settling disputes once resolved by dueling (libel actions for reputation, contract law for business honor, property suits for resource claims). Advocates for anti-dueling statutes, cultivates narratives of dueling as barbarism, and sits on legislative bodies that enact prohibition. The legal profession's expansion is simultaneous with dueling's decline; it has no exit cost and continuous incentive to maintain institutional displacement.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_profession, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_profession, agenda_setter).

% Benefits from a dispute-resolution system grounded in contract law and property rights rather than honor-satisfaction. Industrial capitalism requires predictable courts, stable business relationships, and employees who remain alive to work. Dueling among merchants or entrepreneurs creates business unpredictability and removes capital from productive use. The bourgeoisie support anti-dueling legislation and cultural narratives that frame dueling as incompatible with modernity and rational business practice. They bear minimal enforcement costs and can exit from dueling norms by adopting dignity-culture frameworks that honor business success over personal combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, industrial_bourgeoisie, beneficiary,
    powerful, biographical, mobile, national).

% The Reconstruction apparatus (federal military occupation, state governments installed by Union authority, new federal courts, national commercial law) treats dueling—especially Southern dueling and officer dueling—as a relic of the antebellum order that the war was fought to destroy. Anti-dueling enforcement is part of the broader project of federal institutional consolidation and the suppression of regional honor cultures that had sustained the Confederacy. The institutional order has no exit cost and continuous incentive to suppress practices that symbolize antebellum hierarchy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_institutional_order, beneficiary,
    institutional, generational, analytical, national).

% Face simultaneous legal prosecution (statutes impose fines, imprisonment, or death penalty for dueling), institutional exclusion (military academies discharge officers who duel, civic clubs expel members who participate, professional bodies deny licenses), cultural ostracism (newspapers publish duelists' names, clergy condemn dueling from pulpits), and post-Civil War regional subordination (in the South, dueling is treated as evidence of unreconstructed rebellion). Their identity—as gentlemen, officers, men of honor—is constitutively tied to the practice of dueling: to cease dueling is to accept diminishment and humiliation. They cannot exit by adopting an alternative identity framework without becoming unrecognizable to themselves and their communities. The suppression is both structural (legal and institutional barriers) and internalized (cultural belief that honor-dueling is barbaric delegitimizes the practice from within). Resistance to suppression is high (duelists continue dueling despite prosecution throughout the 19th century), but declining over time as younger generations never adopt the identity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% The Southern plantation gentry were the primary institutional carriers of honor-culture practices and dueling norms. They possessed significant power pre-1865 but were substantially constrained post-Civil War by military occupation, Reconstruction-era legal subordination, and economic displacement. Their regional political power was destroyed; their ability to maintain honor-culture institutions (military academies, exclusive clubs) was broken. Yet they retained enough power to attempt (and fail) to preserve dueling norms in the immediate post-war decades. They experienced the constraint as coercive subordination tied to military defeat rather than as natural cultural evolution—their constrained exit is not voluntary adoption of dignity-culture but forced accommodation to federal authority and institutional hierarchy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_gentry_class, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_gentry_class, observer).

% The doctrine that modernity, progress, and civilization require the replacement of honor-culture with dignity-culture and the replacement of private violence with state-mediated legal order. This framing is vindicated by dueling's disappearance and becomes the dominant cultural narrative by 1920. Newspapers, intellectuals, clergy, and legal theorists all advance the narrative that dueling is barbaric and incompatible with modern civilization. The narrative serves as a legitimizing device for the enforcement machinery and makes the constraints culturally coherent to the benefiting parties. It is not an actor collecting rents, but the ideological structure that makes the other constraints hang together as a unified modernization project.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_modernization_narrative, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_modernization_narrative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, state_apparatus).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling functioned as a dispute-resolution and status-settlement mechanism in honor-culture societies where courts could not adjudicate honor-claims (questions of dignity, insult, seduction, reputation cannot be decided by law; they require satisfaction through acknowledged superior performance). Dueling provided a known, bounded, rapid process to settle these claims: if A insults B (a breach of honor), B can call A to the field, they fight, the victor is vindicated and B's honor is restored (or B dies with honor intact). This coordination function solved a real problem: without it, honor-breaches generate escalating cycles of retaliation with no exit point.
% TRANSFER_FUNCTION: Transfers control of dispute-resolution from private honor-satisfaction (dueling) to state-mediated legal processes and institutional authority structures. The state gains monopoly over legitimate violence and authority to define legitimate injury and appropriate remedy. The legal profession gains authority to adjudicate disputes and collects fees for legal services. The industrial bourgeoisie gain a predictable, law-based dispute system. Post-Civil War federal institutions gain authority to suppress regional honor-culture practices and enforce federal institutional order. Honor-culture practitioners lose the ability to settle grievances via private combat; Southern gentry lose regional political authority and cultural legitimacy for honor-based practices.
% ABSENT_VOICES: Enslaved populations (whose honor had no institutional recognition in either dueling-based or legal-based systems); women (who were central to honor narratives—many duels were fought over women's honor or reputation—but were excluded from combat participation); working-class laborers (for whom honor-dueling was never an available mechanism and whose disputes were never addressed by either constraint); Native American populations (whose own honor and dispute-resolution systems were displaced by both dueling and legal order as part of colonization).
% DISAPPEARANCE_RATIONALE: Historians and the parties themselves disagree on whether dueling's disappearance represents genuine social rearrangement or surface-level cultural shift masking persistent honor-culture dynamics. One reading: the world rearranged—dueling is extinct because the underlying social conditions (honor-culture axioms, gentry dominance, absence of legal alternatives) were fundamentally transformed, and no arrangement of rules could reverse the deeper modernization. Another reading: the world remained largely unchanged—honor-culture persists under new institutional forms (business reputation in lieu of personal honor, legal status in lieu of combat-proven standing), and dueling was simply the external form that had to disappear for the core practice to continue. The overdetermined reading itself encompasses the disagreement: if four independent mechanisms all operated simultaneously, the constraint is overdetermined not because any single mechanism was necessary, but because ALL FOUR together ensured the disappearance regardless of which one was 'really' primary.
% FOUNDING_PROBLEM: Early American gentry society had no institutionalized mechanism for settling honor disputes because courts could not adjudicate matters of personal dignity, and this gap created violent escalation, unpredictable deaths, and social instability within the educated and propertied classes. Dueling emerged as a solution: a known, bounded, rapidly-concluded method to settle honor-claims (insults, seductions, reputation-breaches, questions of courage and standing) and restore social equilibrium through victory or dignified death.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Freeman, Cohen, Burstein) and legal scholars document that by 1880, the founding problem had been substantially solved by alternative institutional mechanisms: libel law replaced reputation-combat (courts could now adjudicate defamation claims), contract law and banking replaced honor-credit relationships, military codes of conduct replaced officer dueling norms, and state monopoly on violence eliminated the legitimacy gap that made private dueling necessary. The founding problem is attested as dead by observers outside the benefiting institutions (academic historians, judges, military reformers who opposed dueling reform); the benefiting parties (state apparatus, legal profession) attest it remains live to justify continued enforcement, but independent evidence contradicts them.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.15→0.62) reflects the accumulation of enforcement mechanisms: early 1750s show minimal state pressure (dueling is tolerated among gentry); by 1850, legal statutes and institutional pressure begin; by 1880, all four mechanisms are active simultaneously. The plateau after 1900 reflects that dueling is culturally extinct, so additional enforcement adds no extractive margin—the constraint has succeeded in its function. Suppression rises sharply 1850–1880 (Civil War era) and plateaus thereafter, indicating the enforcement machinery hardens but does not escalate further. Theater ratio rises after 1850, peaks around 1900, and declines slightly by 1920: the enforcement apparatus becomes increasingly theatrical (prosecutions for dueling honor-codes are rare; enforcement is mostly cultural ostracism and institutional pressure) as the underlying practice dies. The measurement grid is shared across all three metrics, every point authored for every metric at the same six time points.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and legal profession compute the constraint as genuine coordination (replacing dysfunctional private violence with rational-legal order); honor-culture practitioners compute it as pure extraction (suppression of their identity-constitutive practice without consent). The southern gentry occupy a middle ground: they possess power but are constrained by post-Civil War institutional subordination, so they experience the constraint as enforced hierarchy rather than coordination or even ordinary extraction—it is displacement. The engine should compute these seats differently based on power-atom differences (institutional vs. moderate) and exit-option differences (identity_locked for practitioners, constrained for gentry). The contested verdicts across seats reflect the real structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus (institutional, analytical exit) benefits from monopolizing violence and gains legitimacy through the discourse of modernization. Directionality near 0.0 (beneficiary). The legal profession (institutional, analytical exit) benefits from institutionalizing disputes it can charge for. Directionality near 0.1 (slight beneficiary, but less concentrated than state). Industrial bourgeoisie (powerful, mobile exit) benefit from predictable legal mechanisms and bear negligible suppression costs (they were never the primary duelers). Directionality near 0.25 (moderate beneficiary). Honor-culture practitioners (moderate, identity_locked exit) are targets—they face suppression and cannot exit without identity dissolution. Directionality near 0.85 (near-full target). Southern gentry (powerful, constrained exit) are partially targeted—they have power but are constrained by regional subordination post-1865. Directionality near 0.65 (substantial target, but not maximum). The identity-locked exit is critical here: practitioners cannot migrate to a different cultural framework without becoming unrecognizable to themselves; gentry are constrained by military defeat and institutional reconstruction rather than identity fusion, so they retain higher agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to provide an orderly dispute-resolution mechanism for honor-claims that courts could not address. By 1880–1900, this mandate was dead: libel law, contract law, and state monopoly on legitimate violence had solved the underlying coordination problem. Yet enforcement continued (and theaters increased: cultural work to maintain the suppression even as the practice faded). The theater-ratio rise after 1880 reflects exactly this mandatrophy: enforcement becomes increasingly performative because the founding problem is solved and dueling is culturally extinct, yet the machinery persists. The six_questions.founding_problem_status = 'dead' codifies this: the problem dueling solved is gone, but the suppressive constraint remains active. This is why the constraint type is tangled_rope (multiple beneficiary streams from different mechanisms) rather than piton (theatrical inertia): the constraint is still actively enforced, not merely performed. The enforcement is performative in style but real in effect—gentry who duel are prosecuted, military officers who duel are cashiered, civic clubs that tolerate dueling are ostracized. The mechanism is overdetermined, so even as one pathway (cultural narrative) weakens, the others (legal, institutional, post-war hierarchy) maintain pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_separability,
    'Are the four mechanisms (legal prohibition, institutional substitution, cultural shift, Civil War trauma) structurally separable causal pathways, or are they manifestations of a single deeper process (e.g., industrialization/modernization) that generated all four simultaneously?',
    'Comparative historical analysis: examine societies that underwent legal prohibition without Civil War trauma (e.g., Germany, France), institutional substitution without legal change (e.g., Britain''s gradual de-institutionalization of dueling), or cultural shift without legal enforcement (e.g., Scandinavia). If all four occur together even in isolation, they are coupled; if any pathway alone can suppress dueling, overdetermination is confirmed.',
    'If coupled/manifestations of deeper process, the constraint is actually simpler—a single modernization mechanism that appears as four surface phenomena. If separable, overdetermination is confirmed, and the constraint''s ε cannot be decomposed into single-mechanism contributions. Type remains tangled_rope either way, but the framing of beneficiaries shifts (modernization apparatus vs. institutional congeries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_separability, empirical, 'Whether the four mechanisms are independent pathways or unified modernization process.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of dueling primarily structural (legal barriers, institutional exclusion, military enforcement) or internalized (cultural belief that honor-dueling is barbaric, identity shift from honor-culture to dignity-culture)? Or does the measurement conflate both?',
    'Post-suppression trajectory: if legal enforcement ceased but cultural internalization persisted, suppression would remain high (targets believe they deserve the prohibition); if internalization faded after enforcement ended, suppression was primarily structural. Comparative evidence: do societies with legal prohibition but weak cultural shift show higher residual dueling (suggesting structural suppression is weaker) vs. societies with cultural shift and weak legal enforcement?',
    'If suppression is primarily structural, the constraint is externally maintained and would collapse if enforcement ceased. If primarily internalized, targets carry the suppression after exit (post-duel social death). If both, the effective suppression exceeds the structural measure (internalization amplifies structural barriers). The distinction affects the treatment of identity_locked exit: is it structural identity-lock (the agent cannot re-identify without external permission) or internalized identity-lock (the agent cannot re-identify even if permission were granted)?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of dueling is structural or internalized mechanism.').

omega_variable(
    southern_gentry_constrained_vs_trapped,
    'Are Southern gentry constrained (they possess power but face external barriers post-1865) or trapped (their power base was destroyed and they cannot exit the declining honor-culture without economic/social collapse)? The distinction affects directionality computation.',
    'Reconstruction-era historical records: did Southern gentry attempt to maintain dueling norms openly and face military suppression (constrained), or did they acquiesce and internalize defeat, no longer possessing the material basis to duel (trapped)? Did their exit from dueling represent strategic accommodation or resignation?',
    'If constrained, directionality is 0.65–0.75 (powerful agent facing barriers). If trapped, directionality approaches 0.85 (they lost power and cannot maintain the identity). The classification cascades: constrained suggests partial agency and a possible future reclamation (a snare read via constrained exit); trapped suggests the agent is no longer a structural party (they have exited and cannot return, so their role shifts from payer to observer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(southern_gentry_constrained_vs_trapped, empirical, 'Whether post-Civil War Southern gentry were constrained or trapped in their inability to maintain honor-culture practices.').

omega_variable(
    overdetermination_vs_single_dominant_mechanism,
    'Is the causal structure genuinely overdetermined (all four mechanisms would be sufficient independently), or does one mechanism dominate the causal path while others are enabling conditions or artifacts? For instance, did legal prohibition actually suppress dueling, or did cultural shift make legal prohibition unnecessary and the prosecution of duelists is theatrical enforcement of a practice already moribund?',
    'Counterfactual historical analysis (acknowledged as speculative): construct scenarios where each mechanism is removed and assess whether dueling would persist. Examine the sequence of events: which mechanism acted first, and did subsequent mechanisms add suppression or merely reinforce a decision already made? Analyze prosecution rates: are dueling prosecutions concentrated in early decades (legal innovation) or late decades (theater reinforcing cultural death)?',
    'If one mechanism dominates, the constraint is not overdetermined—it is a single mechanism (say, legal prohibition) riding on top of cultural change. Type shifts from tangled_rope to snare or rope depending on which mechanism is dominant. If genuinely overdetermined, the constraint cannot be reduced, and the type remains tangled_rope with multiple beneficiary streams. This omega directly addresses the core claim of the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_vs_single_dominant_mechanism, conceptual, 'Core question: Is the constraint structurally overdetermined or does one mechanism explain the phenomenon?').

omega_variable(
    civilian_vs_military_dueling_divergence,
    'Did military dueling (officer honor-codes) and civilian dueling (gentry honor practices) follow the same suppression trajectory, or did military codes suppress officer dueling while civilian dueling persisted longer? If divergent, are they one constraint or two?',
    'Historical data on prosecutions, military-academy policy changes, and officer conduct records: was military dueling suppressed earlier/faster than civilian dueling? If so, the mechanisms differ (military hierarchy vs. civil legal system), suggesting two constraints, not one.',
    'If convergent trajectory, one composite constraint makes sense. If divergent, this is a decomposition case: write separate stories for military and civilian dueling, link via network.affects_constraints, and route the splitting to an omega in both files. The ε-invariance principle applies: if measuring the constraint one way (military+civilian aggregate) yields different suppression dynamics than measuring it two ways (military alone, civilian alone), the observer is looking at two constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_vs_military_dueling_divergence, empirical, 'Whether military and civilian dueling followed the same suppression trajectory or require separate constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1750, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1850, 0.25).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.42).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.51).
narrative_ontology:measurement(duel_tr_t1920, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1920, 0.48).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.28).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.58).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement(duel_be_t1920, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1920, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1750, 0.22).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1850, 0.52).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.68).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement(duel_su_t1920, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1920, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% The kernel 'dueling_disappearance_mechanism' decomposes into three reading constraints, each instantiating a different causal narrative. The overdetermined_composite_reading (this story) asserts that all four mechanisms (legal, institutional, cultural, traumatic) operated simultaneously and were jointly sufficient. The contraction_reading emphasizes cultural shift (honor→dignity) as primary; the institutional_displacement_reading emphasizes courts outcompeting dueling as a dispute mechanism. These readings are not competing empirical claims about what 'really' happened—they are alternative framings of the same historical sequence, each valid from its reading's epistemic position. This constraint influences its siblings by establishing that single-mechanism readings are incomplete (overdetermination makes all three readings true as partial accounts, but none as complete accounts). No reading forecloses another: historical actors simultaneously experienced legal coercion, institutional pressure, cultural delegitimization, and regional subordination; different historians foreground different mechanisms. The three constraints are linked in a family via this network block.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
