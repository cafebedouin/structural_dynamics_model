% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction via Dueling (Decline Reading)
 *   domain: social/legal/normative
 *
 * SUMMARY:
 *   Dueling persisted across 1600–1900 but at sharply declining frequency and
 *   rising social cost. This reading instantiates the decline hypothesis: the
 *   honor-satisfaction-through-blood constraint remained conceptually
 *   available and continued to govern elite male reputation, but the cost of
 *   participating — legal punishment, professional censure, medical ethics
 *   conflicts, social shame in emerging bourgeois circles — rose steadily.
 *   Duelists still fought, but fewer did, and those who did bore increasing
 *   stigma. By 1900, dueling was fringe, illegal in most jurisdictions, and
 *   treated as an archaic irrelevance by the public discourse — yet the frame
 *   that honor requires blood satisfaction never fully disappeared; it
 *   persisted at the edges, defended by a shrinking honor-bound minority.
 *   This reading emphasizes the persistence-despite-decline pattern, distinct
 *   from the contraction reading (which holds dueling became categorically
 *   unthinkable) and the composite reading (which treats multiple distinct
 *   mechanisms as the real story).
 *
 * KEY AGENTS:
 *   - aristocratic_honor_claimants: The frame-setters; over the interval, their numbers, authority, and social reach contract, but the frame survives.
 *   - duelists_at_risk: Caught in identity-lock; exit requires rejecting honor-through-blood as binding, which most cannot do despite mounting costs.
 *   - state_authorities: Enforce criminalization; their monopoly on violence tightens, making dueling increasingly costly and legally risky.
 *   - bourgeois_emerging_class: Challenge the frame by proposing alternate honor mechanisms (professional prestige, economic achievement); their pressure erodes the frame's universality.
 *   - medical_establishment: Initially complicit (surgeons attend duels); increasingly withdraw as professional ethics crystallize around do-no-harm principles, raising transaction costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction via Dueling (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "social/legal/normative").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '51d37239-6763-4e2c-8abf-381061f7a9ed').
narrative_ontology:cs_kernel_codification('51d37239-6763-4e2c-8abf-381061f7a9ed', fixed_text).
narrative_ontology:cs_authority_grounding('51d37239-6763-4e2c-8abf-381061f7a9ed', lineage).
narrative_ontology:cs_interpretation_layer_present('51d37239-6763-4e2c-8abf-381061f7a9ed').
narrative_ontology:cs_reading_relation('51d37239-6763-4e2c-8abf-381061f7a9ed', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('51d37239-6763-4e2c-8abf-381061f7a9ed', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('51d37239-6763-4e2c-8abf-381061f7a9ed', foundational, honor_persists_as_live_category).
narrative_ontology:cs_axiom_status(honor_persists_as_live_category, holdable).
narrative_ontology:cs_axiom_grounding('51d37239-6763-4e2c-8abf-381061f7a9ed', honor_persists_as_live_category, deontological).
narrative_ontology:cs_axiom('51d37239-6763-4e2c-8abf-381061f7a9ed', secondary, practice_decline_not_cognitive_death).
narrative_ontology:cs_axiom_status(practice_decline_not_cognitive_death, holdable).
narrative_ontology:cs_axiom_grounding('51d37239-6763-4e2c-8abf-381061f7a9ed', practice_decline_not_cognitive_death, empirically_contingent).
narrative_ontology:cs_reference_frame('51d37239-6763-4e2c-8abf-381061f7a9ed', honor_requires_blood_witness).
narrative_ontology:cs_drift_state('51d37239-6763-4e2c-8abf-381061f7a9ed', industrial_bourgeois_era_1850_onward, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51d37239-6763-4e2c-8abf-381061f7a9ed', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, dueling_seconds_and_surgeons).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duelists_at_risk).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, honor_bound_conscripts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, duelists_at_risk).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, honor_is_public_property).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, satisfaction_requires_blood_witness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain social standing through willingness to duel in response to insult. Honor is a currency of aristocratic position, and dueling is the mechanism through which slurs on honor are answered. They set the frame that insult requires blood response, organize seconds, and adjudicate satisfaction. Their identity and social access depend on being known as willing to defend honor by this method.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants, agenda_setter,
    powerful, biographical, identity_locked, national).

% Participate as principals in duels, bearing the mortality and injury risk. Many are aristocratic men whose social position depends on willingness to duel; others are military officers or professionals whose career standing is threatened by a reputation for cowardice if they refuse. They both set the frame and suffer from it. Exit requires rejecting honor-through-blood as conceptually valid, which means accepting social death in their own community.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duelists_at_risk, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, duelists_at_risk, beneficiary).

% Attend duels as seconds (arranging logistics, choosing weapons, determining satisfaction), manage the encounter, or provide medical care. They receive fees, prestige, and employment from the practice. Their professional identity becomes bound to dueling expertise. They have exit options (other medical work, other professions) but operate within a legal/moral framework that treats dueling as a normal social function requiring specialized skill.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_seconds_and_surgeons, beneficiary,
    moderate, biographical, constrained, national).

% Military officers and lower-ranking duelists (merchants, lawyers, professionals gaining social standing) who feel compelled by the honor standard even if not born into aristocracy. They have fewer alternatives than aristocrats and face sharper career or social penalties for declining a challenge. They absorb the mortality risk while reaping less honor benefit than those born into the system.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_bound_conscripts, payer,
    moderate, immediate, trapped, national).

% Initially tolerate dueling (early period) as a way to manage aristocratic violence outside state courts; later criminalize it as enforcement capacity grows and state monopoly on violence is consolidated. They are caught between the honor frame's legitimacy (widely held) and their need to monopolize legitimate violence. Over the interval, state enforcement against dueling increases, making continued participation illegal and socially costly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, state_authorities, observer).

% Rise as an economic power without claim to honor through dueling. They face pressure to adopt dueling to claim aristocratic status, or to reject it as barbaric and assert a new honor frame based on economic and intellectual accomplishment. They are excluded from the original frame but press against its boundaries, creating a legitimacy crisis in the honor standard itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_emerging_class, excluded,
    organized, generational, constrained, national).

% Increasingly professionalizes and codifies its own honor system (medical ethics, do-no-harm principle) that conflicts with the duty to serve as dueling surgeons. This creates an internal contradiction: surgeons are asked to provide medical care specifically to enable violence. Over time, medical professional norms come to treat dueling participation as a violation of professional ethics.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, medical_establishment, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-governed mechanism for resolving honor insults through ritualized combat: both parties accept the outcome (death, injury, or unmarked survival) as binding satisfaction, ending the dispute without vendetta escalation.
% TRANSFER_FUNCTION: Transfers mortality and injury risk to the duelists (equal theoretical exposure, unequal practical by class and skill). Transfers prestige and status to those willing to defend honor by blood. Transfers fees and professional prestige to seconds and surgeons. Over time, transfers legal liability and social stigma to participants.
% ABSENT_VOICES: Bourgeois merchants and professionals who reject the honor frame entirely (absent from the decision-making; they cannot prevent others from dueling but their lack of voice is a silence in favor of the frame). Women whose reputation could be defended by male relatives' duels but who could not duel themselves (excluded from the mechanism but subject to its logic). Medical professionals who oppose dueling on ethical grounds (excluded from the agenda-setting, though some are compelled to participate).
% DISAPPEARANCE_RATIONALE: If dueling vanished entirely by 1900, would the world rearrange? Contested: state authorities and bourgeois observers say no (alternatives exist; other honor mechanisms suffice); the honor-bound minority says yes (blood witness is irreplaceable, honor cannot be satisfied otherwise). The constraint does not fully vanish by 1900; it persists at fringe frequency, suggesting neither camp wins cleanly — the practice declines but doesn't disappear, and the verdict of rearrangement remains undecided.
% FOUNDING_PROBLEM: Early aristocratic societies lacked a neutral third party to adjudicate honor disputes; feudal patrons were interested parties; honor insults had no legitimate remedy outside vendetta. Dueling provided a procedure both parties could accept as neutral and binding: equal risk, witnessed outcome, guaranteed closure.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and legal historians attest that by 1850–1900, state courts provide legal remedy and honor preservation is possible through reputation and professional standing — the original problem is solved. The honor-defending minority attests the problem persists (blood satisfaction is irreplaceable), but this testimony comes from the beneficiary set (those defending the frame itself). Outside observers (bourgeois writers, medical professionals, legal reformers, state prosecutors) consistently attest that the founding problem is obsolete, while duelists persist due to identity-lock and institutional inertia, not because the problem remains unresolved.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The decline reading models dueling as a constraint whose epsilon declines from 0.82 (1600: fully integrated into elite life, low-cost participation, state tolerance) to 0.68 (1900: criminalized, socially costly, fringe participation). Theater ratio rises from 0.25 to 0.58, indicating that by 1900 a growing share of remaining dueling is performative — defending honor symbolically rather than functionally settling disputes. The coercion grid shows the classic piton pattern: accessibility alternatives collapse initially (1600: dueling IS the honor solution, no alternative frame exists) but gradually open (1900: state courts, professional reputation, economic standing all offer alternate honor paths), yet the constraint persists because it remains identity-fused for a shrinking but stubborn minority. Suppression_requirement (state enforcement) rises from 0.35 to 0.72, capturing the state's growing active effort to eliminate the practice. Resistance rises from 0.16 to 0.71, showing the constraint increasingly faces active opposition (legal scholars, medical professionals, bourgeois moralists, state authorities). The shared measurement grid ensures every metric is authored at every time point: no metric-specific grids, no backfill.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic honor-claimants' seat and the state-authorities' seat should diverge sharply. From the aristocratic seat, dueling is a legitimate satisfaction mechanism for genuine disputes; it persists because honor demands persist and no alternative is felt to be equivalent (identity-lock drives this perception). From the state seat, dueling is an illegitimate monopoly on violence that the state must suppress to consolidate its own enforcement authority. By 1900, a third seat (bourgeois emerging class) has crystallized, viewing dueling as barbaric and unnecessary — an honor frame based on economic and intellectual achievement is felt to be superior. The engine computes per-seat types from this structural data: the aristocratic seat perceives rope (coordination of honor disputes); the state seat perceives snare (constraint on its own authority, requiring suppression); the bourgeois seat perceives scaffold (a declining institution that should sunset). The decline reading authorizes all three perceptions as computationally coherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic honor-claimants are beneficiaries (they control the frame, set the standards, receive prestige for defending it) but increasingly face costs (legal risk, reputation damage in bourgeois circles) — their d starts near 0.2 (strong beneficiary position) and drifts toward 0.4 by 1900 (ambiguous position: benefit from status, cost from criminalization and stigma). Duelists are identity-locked victims (the frame binds them; exit is psychologically and socially impossible, not merely constrained) — their d stays high (0.75–0.85) across the interval, as the cost of participation rises without their power to exit. State authorities are neither beneficiary nor victim; they are the constraint's enforcer and opponent — their d is analytical (0.5, observer position) until criminalization hardens, at which point they are de facto targets of the honor frame's claims. The rise in suppression_requirement and the rise in state resistance both indicate growing structural conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (honor disputes without neutral arbiter) appears dead by 1900: state courts provide legal remedy, professional reputation systems provide alternative status, bourgeois economic standing provides another honor frame. Yet the constraint persists, fringe but observable, because the honor-bound minority still holds the original frame as binding. This is the piton signature: the mandate is obsolete, but institutional inertia (identity-lock, professional interest from seconds/surgeons, aristocratic prestige investment) keeps the structure alive through performative maintenance. The theater_ratio rise (0.25 to 0.58) attests this: by 1900, much dueling activity is theatrical honor-defense rather than functional dispute settlement. Mandatrophy is the coherent reading: the constraint satisfies the piton gates (high extraction, high suppression, rising theater, no beneficiary powerful enough to defend it against state and bourgeois pressure, yet it persists). If the founding problem were live, the constraint would be tangled_rope or snare; the fact that it persists at fringe frequency despite being unneeded is what makes it piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_rational_cost_calculus,
    'For the duelists who continued to participate despite rising legal/social costs, was the constraint''s persistence driven by identity-fusion (honor as identity, exit as self-annihilation) or by rational calculation that legal risk and social harm were acceptable prices for honor maintenance?',
    'Correspondence, memoirs, and courtroom testimony from duelists in the late period (1850–1900) that directly addresses their reasoning. Post-exit interviews (where available) with those who abandoned dueling after its primary costs crystallized.',
    'If identity-locked, the constraint''s effective suppression is structural-plus-internalized (the agent carries the suppression even after removal from the coercive environment); if rational cost-acceptance, the suppression is purely structural (state enforcement + social shame). Identity-lock would support the piton classification more strongly; rational cost-acceptance would suggest the constraint might tip into full snare if state enforcement were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_cost_calculus, empirical, 'Whether dueling persistence after high cost reflects identity-fusion or cost-tolerance.').

omega_variable(
    functional_replacement_vs_symbolic_persistence,
    'When state courts, professional reputation, and bourgeois economic honor all became available as alternate dispute-resolution and status mechanisms, did they functionally replace dueling''s coordination role, or did dueling persist because it offered something structurally irreplaceable (ritual witness, blood-price, irrefutable closure)?',
    'Comparative analysis: (a) disputes that would have been settled by dueling in 1650, compared with their resolution method in 1850 (court, mediation, reputation sanction, ignore-and-move); (b) honor narratives in late-period literature and correspondence contrasting dueling honor with economic/professional honor.',
    'If replacement is functional (state courts settle disputes adequately; bourgeois honor substitutes psychologically), the founding problem is genuinely dead and the piton classification is secure. If dueling offered something irreplaceable (ritual closure, blood-witness necessity), then a residual coordination function survived and the constraint might be better classified as a rope or tangled_rope that contracted but didn''t lose function — the terminal type would be unclear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_replacement_vs_symbolic_persistence, conceptual, 'Whether dueling''s decline was functional replacement or symbolic retention of unique meaning.').

omega_variable(
    kernel_reading_boundary,
    'This reading asserts that dueling persisted as a fringe practice at declining frequency until ~1900. The contraction reading asserts it became categorically unthinkable. How do we distinguish between ''rare but conceptually available'' (decline reading) and ''dead as a cultural category, with only historical residue'' (contraction reading)?',
    'Observable distinction: in societies where dueling persisted longest (Germany, Italy), did dueling participants in 1890 understand themselves as defending a live honor norm or as executing an archaic ritual they knew was dying? Did new duelists emerge, or only legacy honor-practitioners? Did the practice adapt and persist in mutated forms (clubs, ceremonies, symbolic challenges without lethal intent) or simply wind down without transformation?',
    'If participants in late dueling understood the norm as live and binding (not archaic), the decline reading holds and epsilon at 1900 should be >0.6. If they understood it as archaic role-play or ritual obligation divorced from genuine honor claims, the contraction reading''s epsilon ≈ 0 is more appropriate. This is the core distinction between the two sibling readings: both observe decline in frequency; they differ on cognitive status (live or dead).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Epistemic status of the honor-through-blood norm in the fringe population: live or archaic?').

omega_variable(
    suppression_as_cause_vs_symptom,
    'Did state criminalization and enforcement suppress dueling (causally drive its decline from below), or did dueling decline for other reasons (bourgeois norm-shift, alternative honor mechanisms) and criminalization was merely symptomatic — enforcement that targets an already-dying practice?',
    'Natural experiment: comparative analysis of jurisdictions with early vs. late criminalization. If dueling declined as sharply in late-criminalizing jurisdictions as in early ones, suppression is symptom; if early-criminalizing jurisdictions show faster decline, suppression is cause.',
    'If suppression is cause, then the rising suppression_requirement in the measurements series (0.35 to 0.72) is doing real causal work in the decline. If suppression is symptom, then suppression_requirement is a consequence of decline, not its driver, and the real mechanism lies elsewhere (bourgeois norm-shift, alternative honor mechanisms, insurance markets). This affects how we attribute the decline and the classification of the constraint''s relationship to state authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_as_cause_vs_symptom, empirical, 'Whether state enforcement caused dueling''s decline or merely responded to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1700, 0.32).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.39).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.47).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1850, 0.54).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.58).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1600, 0.82).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1700, 0.78).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.74).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.71).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1850, 0.69).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1700, 0.45).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.54).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.63).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(hono_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 1600, 0.85).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 1900, 0.35).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 1600, 0.88).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 1900, 0.42).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 1600, 0.92).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 1900, 0.38).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 1600, 0.91).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 1900, 0.36).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_mechanism__decline_reading, resistance(class), 1600, 0.14).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_mechanism__decline_reading, resistance(class), 1900, 0.68).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_mechanism__decline_reading, resistance(individual), 1600, 0.18).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_mechanism__decline_reading, resistance(individual), 1900, 0.69).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 1600, 0.22).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 1900, 0.72).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_mechanism__decline_reading, resistance(structural), 1600, 0.16).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_mechanism__decline_reading, resistance(structural), 1900, 0.71).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 1600, 0.84).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 1900, 0.51).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 1600, 0.82).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 1900, 0.48).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 1600, 0.78).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 1900, 0.44).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 1600, 0.79).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 1900, 0.47).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_mechanism__decline_reading, suppression(class), 1600, 0.38).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_mechanism__decline_reading, suppression(class), 1900, 0.76).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_mechanism__decline_reading, suppression(individual), 1600, 0.31).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_mechanism__decline_reading, suppression(individual), 1900, 0.74).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 1600, 0.28).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_mechanism__decline_reading, suppression(structural), 1600, 0.25).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_mechanism__decline_reading, suppression(structural), 1900, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel spawns three constraint stories, each a different reading: decline_reading (this file) treats dueling as persisting at declining frequency under mounting costs, remaining conceptually available to the identity-locked minority; contraction_reading treats it as becoming categorically unthinkable; composite_reading treats multiple simultaneous mechanisms (state monopoly, bourgeois norm-shift, insurance, cognitive drift) as the real story. Each has its own epsilon, stakeholder structure, and type. They are linked by network.affects_constraints and distinguished by omega variables routing the kernel contest to the committer layer (cs_structure.reading_relations and axioms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
