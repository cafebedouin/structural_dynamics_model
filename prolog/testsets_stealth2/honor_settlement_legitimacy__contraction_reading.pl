% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Cognitive Closure of Honor Settlement (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   Between roughly 1780 and 1920 the European honor-settlement order
 *   transformed from a live, peer-enforced obligation (an insult demanded a
 *   challenge on pain of social death) into a closed cognitive space in which
 *   dueling is not merely prohibited but incomprehensible as legitimate
 *   action. This story instantiates the contraction_reading of the
 *   honor_settlement_legitimacy kernel: the demise was a cognitive event —
 *   honor culture itself exited the normative possibility space — rather than
 *   a persistence story (drop_reading) or an overdetermination story
 *   (composite_reading). The epsilon referent throughout is the standing
 *   honor-settlement arrangement as this reading assesses it: at interval
 *   start that arrangement is the live honor code binding gentlemen; at
 *   interval end it is the closure itself. The claim and the metrics are
 *   independently authored: the claim (rope) states what I believe is
 *   structurally true of the standing arrangement — a genuine
 *   collective-action problem solved with near-zero coercive overhead — while
 *   the metrics describe what I believe is descriptively true; they happen to
 *   agree here, but neither was tuned toward the other or toward a predicted
 *   engine output. KEY AGENTS (by structural relationship): -
 *   dueling_gentleman_class: Primary target under the code era
 *   (powerful/trapped) — bore the challenge-bind's burdens; dissolved into
 *   the beneficiary public as the framework transformed -
 *   honor_culture_tradition_bearers: Residual target
 *   (organized/identity_locked) — the seat whose vanishing IS the
 *   contraction; bore foreclosure of a life-form -
 *   centralizing_state_apparatus: Agenda-setter and secondary beneficiary
 *   (institutional/mobile) — prohibition attempts failed; collected
 *   jurisdiction as closure completed - rising_bourgeois_professionals:
 *   Primary beneficiary (organized/mobile) — entered public life without
 *   honor-field competition; carriers of the new framework -
 *   wider_disputant_public: Beneficiary (powerless/mobile) — spared the
 *   challenge-bind entirely - legal_professional_class: Beneficiary
 *   (organized/mobile) — absorbed dispute settlement into priced jurisdiction
 *   - historical_sociologists: Analytical observer (analytical/analytical) —
 *   sees the full structure; produces the competing readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.12).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.06).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Cognitive Closure of Honor Settlement (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '0f7fc175-fbba-45ae-a75a-ab41912e4556').
narrative_ontology:cs_kernel_codification('0f7fc175-fbba-45ae-a75a-ab41912e4556', distributed).
narrative_ontology:cs_authority_grounding('0f7fc175-fbba-45ae-a75a-ab41912e4556', distributed).
narrative_ontology:cs_reading_relation('0f7fc175-fbba-45ae-a75a-ab41912e4556', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('0f7fc175-fbba-45ae-a75a-ab41912e4556', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('0f7fc175-fbba-45ae-a75a-ab41912e4556', foundational, legitimacy_is_framework_constituted).
narrative_ontology:cs_axiom_status(legitimacy_is_framework_constituted, holdable).
narrative_ontology:cs_axiom_grounding('0f7fc175-fbba-45ae-a75a-ab41912e4556', legitimacy_is_framework_constituted, empirically_contingent).
narrative_ontology:cs_axiom('0f7fc175-fbba-45ae-a75a-ab41912e4556', secondary, cognitive_closure_requires_no_enforcement).
narrative_ontology:cs_axiom_status(cognitive_closure_requires_no_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('0f7fc175-fbba-45ae-a75a-ab41912e4556', cognitive_closure_requires_no_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('0f7fc175-fbba-45ae-a75a-ab41912e4556', post_contraction_normative_order).
narrative_ontology:cs_drift_state('0f7fc175-fbba-45ae-a75a-ab41912e4556', contemporary_post_honor_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0f7fc175-fbba-45ae-a75a-ab41912e4556', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, rising_bourgeois_professionals).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, centralizing_state_apparatus).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, wider_disputant_public).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_professional_class).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, dueling_gentleman_class).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_culture_tradition_bearers).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, weberian_violence_monopoly_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, eliasian_civilizing_process_thesis).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, judicial_supremacy_in_dispute_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officers, gentry, and gentlemen of the eighteenth and nineteenth centuries bound by the honor code: an insult obliged a man to demand a challenge or lose standing among his peers. The class enforced the obligation on itself — no external authority ran the system; seconds, codes duello, and peer opinion did. Bearing its burdens meant risking death or ruin in a field a man could not refuse to enter; refusing meant social death. Across the nineteenth century their descendants stopped comprehending the obligation: the vocabulary of satisfaction, point of honor, and apology by firearms drained out of their letters, memoirs, and conduct manuals until the bind had no one left to bind.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, dueling_gentleman_class, payer,
    powerful, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, dueling_gentleman_class, agenda_setter).

% The men who kept the honor ethic alive longest — dueling-fraternity members, officer casts in regions where the code lingered, traditionalist circles. Their self-understanding was fused with the ethic: to renounce the field was to become someone else, so exit was not a choice they could recognize from inside. They faced a world in which the words they needed — satisfaction, cowardice, the field — no longer moved anyone outside their circles. By the interval's end they were scattered pockets whose challenges drew laughter or police rather than seconds, and their children inherited the new framework intact.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_tradition_bearers, payer,
    organized, generational, identity_locked, regional).

% Monarchies and then republics that criminalized dueling from the seventeenth century onward while their own officer corps kept fighting duels. Edicts, courts-martial, and pardons-for-winners failed for two centuries; the state's prohibitions never produced the end of the practice. The end arrived through cultural change the state then ratified. As quarrels moved into courts, the state gained jurisdiction, court fees, and an uncontested monopoly on lawful force — gains it had sought by decree and received instead by transformation it did not direct.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, centralizing_state_apparatus, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, centralizing_state_apparatus, beneficiary).

% Lawyers, doctors, merchants, journalists, and officials ascending through the nineteenth century. Under the old code they were perpetual outsiders — men who could be insulted by their betters without recourse, since the field of honor was gated by birth and rank. The framework transformation deleted the field itself: public standing came to rest on credentials, office, and printed opinion rather than prowess at arms. They staffed the schools, presses, and professions that taught the new framework, and their sons inherited it as common sense.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, rising_bourgeois_professionals, beneficiary,
    organized, biographical, mobile, national).

% Everyone else touched by quarrels — tradesmen, neighbors, families, soldiers below officer rank. Before the transformation, honor logic leaked downward: tavern fights over satisfaction, militia challenges, urban knife duels. After it, a quarrel was a matter for apology, lawsuit, or forgetting. Nothing binds this public to the arrangement; they simply inhabit a world where the old option does not occur to anyone as a thing one might do.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, wider_disputant_public, beneficiary,
    powerless, biographical, mobile, national).

% Judges, barristers, and legal scholars whose jurisdiction expanded as private settlement collapsed. Defamation, assault, and insult — once matters for seconds — became causes of action with fees attached. The profession supplied the doctrinal account of law as the civilized substitute for the duel that gave the new framework its self-description, and it collected a durable market in dispute resolution.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_professional_class, beneficiary,
    organized, biographical, mobile, national).

% Scholars of honor, violence, and state formation who reconstruct the transformation from archives, conduct manuals, court records, and memoirs. They hold the competing readings of what happened and why, and their disagreements define the kernel's contest. They bear none of the arrangement's costs and collect none of its gains.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces the honor-violence equilibrium for dispute settlement: where every gentleman once stood bound to answer an insult with a challenge or lose standing, the transformed framework routes all quarrels through law, apology, negotiation, or deliberate disregard — dissolving the mutual-assured-challenge problem by deleting the challenge option from every party's action space at once.
% TRANSFER_FUNCTION: Moves dispute-settlement authority from private honor adjudication (seconds, codes, the field) to public legal institutions; renders the aristocracy's distinction capital worthless; moves the physical risk of quarrels from duelists to almost no one. Mostly the arrangement destroys a transfer system rather than building one — the mutual-disarmament dividend accrues to no seat.
% ABSENT_VOICES: The tradition-bearers who experienced the closure as foreclosure — men whose entire ethical vocabulary assumed the field of honor — left few organized voices; their testimony survives in memoirs of no longer understanding themselves. Also absent: the people killed or ruined under the old code, who might have testified against it but never had a seat in its councils.
% DISAPPEARANCE_RATIONALE: If the closure vanished overnight — if duel-legitimacy returned to the action space — legal systems, professional ethics, military discipline, insurance, and masculine norms would all rearrange immediately: courts would lose their monopoly on quarrel-resolution, officers would face renewed challenge obligations, and every institution built on the assumption that insults are litigation matters, not affairs of honor, would need rebuilding.
% FOUNDING_PROBLEM: The honor system's own crisis: by the eighteenth century the challenge-bind was killing disproportionate numbers of officers and gentlemen, exposing them to blackmail, and colliding with state legal authority. The transformation answered the problem of escaping a self-enforcing violence equilibrium that its own participants could not unilaterally exit — defection meant social death, so only a change in the shared framework could release everyone simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: eighteenth- and nineteenth-century anti-dueling writers and clergy attested the bind's costs while opposing the practice; military reformers documented officer losses; coroner and consistory-court records preserve the deaths and scandals; and twentieth-century historians (Kiernan, Elias, Frevert, Nye) reconstructed the equilibrium from archival evidence independent of any beneficiary's account. No corroboration rests on the closure's beneficiaries alone.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).
:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends low (0.12) because the standing arrangement — the closure — imposes only residual costs: the foreclosed life-form of the last tradition-bearers and the modest concentration of dispute-authority (and fees) in legal institutions. Suppression is very low (0.06) because nothing enforces the closure; dormant anti-dueling statutes are never invoked, and no one polices thoughts about honor. Theater is low (0.10) and the series is nearly flat by design of the phenomenon: the visible theater of the period — first-blood rituals, late French political duels staged for the press — belongs to the DYING practice performing a code its actors no longer believed, not to the closure, which is maintained by nothing at all. That absence of maintenance activity is the contraction signature and distinguishes this account from prohibition-based ones, which require continuous enforcement effort. Accessibility collapse is high (0.90) despite the rope claim: once inside the transformed framework, the duel-option is cognitively absent rather than merely forbidden — but this does not mean rival coordination mechanisms were crushed; apology, law, mediation, and disregard remain abundantly available, so the collapse is specific to the foreclosed option. Resistance is near zero (0.03): no movement ever organized to restore the right of challenge. The measurement series run on ONE shared grid (1780/1810/1840/1870/1900/1920) with every tracked metric authored at every point. Base extractiveness declines monotonically (0.68 to 0.12) as the bind's burden lifts; the suppression_requirement series is a deliberate falling trajectory (0.70 to 0.06) modeling enforcement DECAY — the early arrangement needed constant peer policing and failed state prohibition, and the requirement fell as internalization completed — which is the sanctioned use of that series, not a static-suppression case. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. The scalars in base_properties describe the interval-end state and match the final grid points.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently along BOTH the seat axis and the time axis. The dueling_gentleman_class seat at interval start sits trapped inside peer-enforced coercion — from that position the arrangement computes as something far harsher than coordination, closer to a bind its holders could not exit. By interval end that same lineage has dissolved into the beneficiary public and experiences the closure as background freedom. The honor_culture_tradition_bearers seat keeps high target-directionality throughout: for them the closure is foreclosure of a constitutive identity, and they are the only seat for whom the arrangement's end-state carries real cost. The state seat attempted agenda-setting by decree for two centuries, failed, and then collected incidental jurisdictional gains — its computed position should sit nearer the beneficiary end than its enforcement history suggests. The engine computes these per-seat classifications from the structural data; the divergence between the code-era experience and the closure-era experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The two victim groups carry high target-directionality: dueling_gentleman_class (victim, trapped exit — no way to refuse a challenge without ruin) and honor_culture_tradition_bearers (victim, identity_locked — exit means becoming someone else, the deepest lock in the story). The four beneficiary groups carry low directionality: rising_bourgeois_professionals and legal_professional_class (mobile exit, direct gains), wider_disputant_public (diffuse safety gains), centralizing_state_apparatus (agenda-setter with secondary beneficiary position — modest jurisdictional collection, partially offset by its failed enforcement expenditures). historical_sociologists sit at the analytical neutral point. No directionality overrides are authored: the derivation from declarations plus exit options captures every seat's relationship without correction. Note on coalition failure: the victim seats never formed a viable coalition — the tradition-bearers' identity lock fragmented them into regional pockets and the gentleman class's own peer enforcement turned each member into an enforcer against the others — which is part of why measured resistance stayed near zero despite real costs borne.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (escaping the self-enforcing honor-violence equilibrium) is dead: the population that faced it no longer exists, and the closure persists without it. Authored honestly, this yields the founding_problem_status x disappearance_verdict mismatch (dead + world_rearranges) that flags capture/zombie candidates — but the surrounding structure argues the flag is benign here. A zombie or captured constraint shows concentrated gain receipt, theatrical maintenance, or an administrator profiting from inertia; this arrangement shows gain_flow diffuse (checked across every seat — the mutual-disarmament dividend lands nowhere in particular, and the state's court-fee collections are second-order and bounded), near-zero theater, no administrator at all, and a mandate fulfilled BY the arrangement's own persistence: preventing honor violence is not a function the closure has atrophied away from, it is what the closure IS. The contrast with a piton is exact: a piton's function has died while its form persists; here the function is fully live at zero overhead. The mismatch flag should therefore resolve as completed-coordination signature, not mandatrophy — the constraint outlived its founding problem because it solved it so thoroughly that the problem's bearers ceased to exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_residual_comprehension,
    'Did any population retain live comprehension of dueling as legitimate settlement at the interval''s end, or did the framework transform every bearer of honor culture?',
    'Systematic coding of late-period sources — memoirs, dueling-fraternity records, challenge incidents 1890-1930 — for whether participants treated the duel as live legitimacy or as quotation and performance.',
    'If residual live comprehension existed at scale, the contraction reading overstates closure, its foreclosure edge to the drop reading is mis-authored, and the corpus should re-center on the drop reading''s residual-persistence structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_residual_comprehension, empirical, 'Whether cognitive closure was total or left a comprehending residue (contraction vs drop readings of the kernel).').

omega_variable(
    sufficiency_vs_overdetermination,
    'Was framework transformation sufficient to end dueling, or one reinforcing strand among legal prohibition, economic change, military technology, and demographics (the composite reading)?',
    'Comparative counterfactual history across jurisdictions pairing prohibition with different cultural trajectories — German student corpora persisting under legality, French political duels persisting under prohibition, British extinction without serious prohibition — to isolate the cultural variable''s causal weight.',
    'If transformation was insufficient alone, this constraint''s persistence-basis includes legal and economic supports the story does not model, and its classification should be read as one strand of a composite family rather than a standalone coordination achievement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_vs_overdetermination, empirical, 'Whether the contraction mechanism sufficed or the decline was overdetermined.').

omega_variable(
    naturalized_vs_reproduction_dependent,
    'Is the closure now a self-maintaining fixture of modern cognition, or does it depend on continued institutional reproduction (schooling, courts, professional ethics) that could lapse?',
    'Examine societies and periods where state and educational institutions collapsed or never covered the population: did honor-violence logics re-emerge where the reproduction machinery was absent?',
    'If reproduction-dependent, the arrangement is a maintained coordination mechanism whose true cost includes real upkeep the low suppression score hides; if naturalized, it trends toward mountain-like fixity and the low suppression reflects permanence rather than absence of need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalized_vs_reproduction_dependent, conceptual, 'Whether the closure is self-maintaining or institutionally reproduced — the constructed-versus-natural-law ambiguity for this arrangement.').

omega_variable(
    identity_lock_dissolution_path,
    'How did the tradition-bearers'' identity fusion with the honor ethic dissolve without organized resistance — conversion within lifetimes, or generational replacement of the fused cohort?',
    'Cohort analysis of honor vocabulary across generations in correspondence, autobiographies, and officer-corps intake records.',
    'Generational replacement would mean the closure never faced its strongest possible opponents at full strength, weakening claims about its robustness against a motivated adherent class; within-lifetime conversion would strengthen them and bear on reversibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_path, empirical, 'Mechanism by which identity-locked adherence to the honor ethic ended.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsl_contraction_tr_t1780, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1780, 0.06).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1780, observed).
narrative_ontology:measurement(hsl_contraction_tr_t1810, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1810, 0.07).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1810, observed).
narrative_ontology:measurement(hsl_contraction_tr_t1840, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1840, observed).
narrative_ontology:measurement(hsl_contraction_tr_t1870, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1870, 0.09).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1870, observed).
narrative_ontology:measurement(hsl_contraction_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1900, observed).
narrative_ontology:measurement(hsl_contraction_tr_t1920, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement_basis(hsl_contraction_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(hsl_contraction_be_t1780, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1780, 0.68).
narrative_ontology:measurement_basis(hsl_contraction_be_t1780, observed).
narrative_ontology:measurement(hsl_contraction_be_t1810, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1810, 0.56).
narrative_ontology:measurement_basis(hsl_contraction_be_t1810, observed).
narrative_ontology:measurement(hsl_contraction_be_t1840, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1840, 0.41).
narrative_ontology:measurement_basis(hsl_contraction_be_t1840, observed).
narrative_ontology:measurement(hsl_contraction_be_t1870, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1870, 0.29).
narrative_ontology:measurement_basis(hsl_contraction_be_t1870, observed).
narrative_ontology:measurement(hsl_contraction_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.17).
narrative_ontology:measurement_basis(hsl_contraction_be_t1900, observed).
narrative_ontology:measurement(hsl_contraction_be_t1920, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement_basis(hsl_contraction_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(hsl_contraction_su_t1780, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1780, 0.7).
narrative_ontology:measurement_basis(hsl_contraction_su_t1780, observed).
narrative_ontology:measurement(hsl_contraction_su_t1810, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1810, 0.62).
narrative_ontology:measurement_basis(hsl_contraction_su_t1810, observed).
narrative_ontology:measurement(hsl_contraction_su_t1840, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1840, 0.48).
narrative_ontology:measurement_basis(hsl_contraction_su_t1840, observed).
narrative_ontology:measurement(hsl_contraction_su_t1870, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1870, 0.32).
narrative_ontology:measurement_basis(hsl_contraction_su_t1870, observed).
narrative_ontology:measurement(hsl_contraction_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement_basis(hsl_contraction_su_t1900, observed).
narrative_ontology:measurement(hsl_contraction_su_t1920, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1920, 0.06).
narrative_ontology:measurement_basis(hsl_contraction_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the end of dueling' covers three structurally distinct claims about the same transformation and is authored as three stories sharing the honor_settlement_legitimacy kernel. This contraction_reading authors epsilon for the standing arrangement as total cognitive closure (low extraction, no enforcement, high accessibility collapse); the drop_reading authors epsilon for a world with a comprehending residual population (higher suppression of a live minority practice); the composite_reading authors epsilon for an overdetermined decline (causal weight distributed across mechanisms). The readings differ in victim sets and in what would count as falsifying evidence, so no single story can carry all three epsilons. Upstream/downstream: the composite reading cites the contraction mechanism as one component, giving this story a partial upstream evidentiary role, while the drop reading competes directly with this story's foreclosure claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
