% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bifurcated Sovereignty: Shogunal Delegation of Imperial Mandate (Bakufu Reading)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This story authors the bakufu delegation reading of the imperial mandate
 *   kernel: the emperor's mandate-granting function is treated as
 *   structurally separable from the exercise of governance, licensing the
 *   shogunate and samurai class to rule as the emperor's institutionally
 *   sanctioned delegates across multiple regime changes (Kamakura, Ashikaga,
 *   Tokugawa). On this reading the arrangement is a hybrid: it genuinely
 *   solves a coordination problem (sacralized continuity of legitimacy
 *   despite recurring transfers of actual governing power) while
 *   simultaneously extracting quiet, durable advantage for the shogunal house
 *   and samurai class, who capture governing authority and revenue while
 *   confining the emperor to a politically inert ceremonial role they
 *   actively maintain because it is the ultimate source of their own
 *   legitimacy. The theater ratio rises across the interval as the
 *   shogunate's active management of imperial ceremony and succession (court
 *   appointments, forced abdications, marriage politics) increasingly serves
 *   shogunal control rather than any independent religious function — a
 *   maturing performance of deference layered atop an increasingly settled
 *   power asymmetry. This is NOT the loyalist_restoration_reading, which
 *   holds mandate and governance to be inseparable; that is a sibling
 *   constraint with its own ε, its own beneficiary/victim structure, and its
 *   own file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bifurcated Sovereignty: Shogunal Delegation of Imperial Mandate (Bakufu Reading)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'b714d24f-3b28-427c-bd02-1d126e314557').
narrative_ontology:cs_kernel_codification('b714d24f-3b28-427c-bd02-1d126e314557', distributed).
narrative_ontology:cs_authority_grounding('b714d24f-3b28-427c-bd02-1d126e314557', practice).
narrative_ontology:cs_interpretation_layer_present('b714d24f-3b28-427c-bd02-1d126e314557').
narrative_ontology:cs_reading_relation('b714d24f-3b28-427c-bd02-1d126e314557', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('b714d24f-3b28-427c-bd02-1d126e314557', foundational, mandate_delegability_is_legitimate).
narrative_ontology:cs_axiom_status(mandate_delegability_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b714d24f-3b28-427c-bd02-1d126e314557', mandate_delegability_is_legitimate, conventional).
narrative_ontology:cs_axiom('b714d24f-3b28-427c-bd02-1d126e314557', secondary, ritual_and_administrative_headship_are_severable_offices).
narrative_ontology:cs_axiom_status(ritual_and_administrative_headship_are_severable_offices, holdable).
narrative_ontology:cs_axiom_grounding('b714d24f-3b28-427c-bd02-1d126e314557', ritual_and_administrative_headship_are_severable_offices, conventional).
narrative_ontology:cs_reference_frame('b714d24f-3b28-427c-bd02-1d126e314557', sacral_dynastic_continuity_framework).
narrative_ontology:cs_drift_state('b714d24f-3b28-427c-bd02-1d126e314557', late_tokugawa_imperial_revival_pressure, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b714d24f-3b28-427c-bd02-1d126e314557', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_house).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasant_and_commoner_classes).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_loyalist_factions).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, provincial_daimyo_outside_bakufu_favor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises actual administrative, military, and fiscal authority over the realm under a commission (seii taishogun) formally conferred by the emperor. Sets law (buke shohatto and equivalents), commands the samurai hierarchy, and manages foreign and domestic policy. Maintains the emperor's ritual primacy precisely because that primacy is the source instrument that legitimizes the shogunate's own rule; has every incentive to keep the emperor ceremonially exalted and politically inert.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunal_house, agenda_setter,
    institutional, generational, arbitrage, national).

% Retains prestige, court rank, ritual function, and a stipend administered by the bakufu, in exchange for confining its activity to ceremony, scholarship, and the arts. Benefits from continued relevance and material support but pays through near-total loss of independent political agency and close bakufu surveillance of court affairs (e.g. via a shogunal deputy resident in Kyoto).
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility, payer).

% Constitutes the actual administrative and military stratum that governs under the shogunate's delegated authority. Draws stipends, land rights, and social status from the arrangement; its legitimacy as a ruling class depends entirely on the imperial mandate flowing downward through the shogun's commission. Has no plausible exit from the system that grants it standing.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_class, beneficiary,
    organized, generational, constrained, national).

% Hold real regional military and economic power but are subordinated within the delegated hierarchy, subject to hostage systems, forced attendance, and reassignment at the shogunate's discretion. Their local authority is itself downstream of the same legitimacy chain that empowers the shogun, so open resistance risks being cast as rebellion against the mandate itself rather than mere political dissent.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, provincial_daimyo_outside_bakufu_favor, payer,
    powerful, biographical, trapped, regional).

% Bear the material weight of the entire delegated order — taxation, corvée, and status subordination — while having no access to the legitimacy discourse that governs them. The mandate's bifurcation into ritual and administrative heads is invisible to their daily situation except as the reason obedience is owed to samurai authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasant_and_commoner_classes, payer,
    powerless, biographical, trapped, local).

% Hold that the emperor's legitimacy cannot be delegated away from active governance; argue the shogunate's administrative supremacy is usurpation dressed in borrowed sanctity. Structurally excluded from the delegation arrangement's own self-description, since the arrangement's stability depends on treating this objection as illegitimate or seditious rather than as a live constitutional claim.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_loyalist_factions, excluded,
    moderate, civilizational, identity_locked, national).

% Examine bifurcated-sovereignty arrangements (imperial Japan, Holy Roman Empire's spiritual/temporal split, constitutional monarchies with delegated executive authority) as a comparative category, without a stake in which reading of the mandate is correct.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, comparative_constitutional_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates the source of legitimacy (a claim of unbroken, sacred, dynastic continuity) from the exercise of governing power (which requires military capacity, administrative competence, and rapid decision-making the ritual office cannot itself supply), allowing governing authority to pass across shogunal houses and regime changes without requiring a rupture in the legitimacy chain each time.
% TRANSFER_FUNCTION: Moves real governing authority, revenue extraction rights, and coercive capacity from the imperial office to the shogunal house and the samurai class beneath it, while moving ceremonial deference, material stipends, and a formally superior (but practically powerless) rank upward to the emperor and court nobility in return.
% ABSENT_VOICES: Imperial loyalist factions who hold that mandate and governance cannot be separated are structurally outside this reading's own account of itself; their objection is treated by the delegation arrangement as sedition or nostalgia rather than as a live constitutional argument. Commoners bearing the material cost of the arrangement have no voice in the legitimacy discourse at all.
% DISAPPEARANCE_RATIONALE: If the delegation fiction dissolved, the shogunal house's entire claim to rule would lose its formal warrant overnight, provincial daimyo would face an open question about whose authority binds them, the samurai class's social rank would lose its legitimating anchor, and the question of who actually governs would have to be settled by force or new constitutional invention rather than inherited ceremony — this is close to what in fact occurred at the Meiji restoration.
% FOUNDING_PROBLEM: A durable, sacralized claim to rule (the imperial line) needed to be reconciled with the practical reality that effective governance in a large, contested, militarized polity requires concentrated administrative and military command that a largely ceremonial court could not itself exercise or defend.
% FOUNDING_PROBLEM_CORROBORATION: The shogunal house and its administrative apparatus attest the delegation is a stable and necessary division of labor. Imperial loyalist historians and, later, Meiji-era restoration ideologues attest from outside the bakufu's own self-justification that the founding problem was in fact resolved or never genuinely required separation, and that the delegation persisted primarily because it served the shogunate's interest in insulating itself from direct accountability to the sacred office it invoked.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the durable, quiet capture of governing surplus by the shogunal house and samurai class under cover of a coordination story about sacred continuity — moderate rather than severe because a genuine coordination function (avoiding legitimacy rupture at every regime transition) is real and load-bearing. Suppression (0.71) is high because the arrangement depends on actively foreclosing the loyalist counter-reading — treating imperial political activism as illegitimate — and on coercive management of provincial daimyo and commoners who have no standing in the legitimacy discourse. Theater ratio (0.62) is high and rising because increasing administrative sophistication in managing imperial ceremony (court rank distribution, succession control) is substituting maintenance-of-appearance for the sacral function it claims to serve.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunal house sits nearest the beneficiary end: it authors the rules, commands the coercive apparatus, and its exit options are effectively arbitrage-grade (it can reshape the delegation's terms at will). The samurai class and kyoto court nobility are secondary beneficiaries with constrained exit — their standing depends on the arrangement continuing, so they cannot exit without dissolving their own legitimacy. Peasants and provincial daimyo outside favor sit toward the target end: trapped, bearing material and status costs, with no access to the legitimacy discourse that justifies their subordination. Imperial loyalist factions are treated as identity_locked rather than simply excluded-and-mobile: their entire political identity is constituted by rejecting the delegation's separability claim, so for them exit from the constraint would mean abandoning the loyalist reading itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling sacred continuity with practical governance) was genuinely live at the outset of shogunal rule and arguably remains partially live even late in the interval — hence founding_problem_status is authored as contested rather than flatly dead. This prevents mislabeling the entire arrangement as pure extraction: there is a real coordination good being solved (institutional continuity across violent regime transitions is not free). But the corroboration record shows the shogunate's own account of the problem's continued necessity is not independently corroborated by outside observers, who increasingly see the delegation as serving primarily to insulate the shogunate from direct accountability — which is why this reading computes as tangled_rope rather than either pure rope or pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_separability_ambiguity,
    'Is the separation of mandate-granting from governing function a genuine, stable constitutional distinction, or is it a legitimating fiction the shogunate maintains because it is structurally convenient — i.e., would the mandate concept survive contact with an emperor who actually attempted to exercise governing authority?',
    'Historical episodes where emperors did attempt active political intervention (e.g. the Kenmu Restoration attempt, late-Tokugawa imperial court activism) test whether the delegation framework treats such attempts as a live exercise of an always-latent authority or as illegitimate overreach requiring suppression — the pattern of response is diagnostic.',
    'If active imperial governance is met with suppression or reframing as aberration whenever attempted, the separability claim functions primarily as legitimating cover for shogunal extraction rather than as a genuine, symmetric constitutional division of labor, pushing the classification further toward snare; if such attempts are genuinely accommodated within the delegation framework without crisis, the coordination reading is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_separability_ambiguity, conceptual, 'Whether delegation is a stable division of labor or a convenient fiction tested only by crisis.').

omega_variable(
    committer_frame_bifurcation,
    'This story instantiates the bakufu_delegation_reading of the imperial_mandate kernel; the sibling loyalist_restoration_reading holds mandate and active governance to be inseparable. Where exactly does the disagreement locate structurally — is it about the definition of ''mandate'' itself, about whether delegation can be legitimate in principle, or about whether THIS PARTICULAR delegation (shogunal) satisfies whatever legitimate-delegation criteria might exist?',
    'Comparative analysis of the two readings'' respective axioms: bakufu_delegation_reading holds mandate_delegability_is_legitimate as foundational; loyalist_restoration_reading would hold its negation. The disagreement is located at the level of whether delegation as such can preserve legitimacy, not at the level of factual governance competence.',
    'If the disagreement is purely definitional (about what ''mandate'' means), the two readings can coexist as different but internally consistent frameworks each party actually holds. If it is a factual dispute about whether THIS shogunate satisfied delegation criteria, resolution might in principle favor one reading over the other on evidentiary grounds even while the general delegability question remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_bifurcation, conceptual, 'Where exactly the bakufu and loyalist readings'' disagreement is structurally located.').

omega_variable(
    theatricality_versus_genuine_sacrality,
    'Is the rising theater_ratio evidence that the sacral function was always instrumentally maintained by whoever held power, or evidence of genuine sacral belief gradually eroding into pure political management over the interval?',
    'Contemporaneous court diaries, religious ritual records, and popular devotional practice toward the imperial institution across the interval would distinguish sincere belief from institutionally maintained performance.',
    'If sacral belief was always instrumentally deployed, the tangled_rope classification is stable from the outset; if genuine belief eroded over time, the constraint may have transitioned from a more rope-like arrangement early on toward a more extractive one later, which the temporal measurements are designed to detect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theatricality_versus_genuine_sacrality, empirical, 'Whether the sacral function''s theatricality was constant or emerged over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(impe_tr_t50, imperial_mandate__bakufu_delegation_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.46).
narrative_ontology:measurement(impe_tr_t150, imperial_mandate__bakufu_delegation_reading, theater_ratio, 150, 0.53).
narrative_ontology:measurement(impe_tr_t200, imperial_mandate__bakufu_delegation_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(impe_tr_t250, imperial_mandate__bakufu_delegation_reading, theater_ratio, 250, 0.62).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(impe_be_t50, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.53).
narrative_ontology:measurement(impe_be_t150, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(impe_be_t200, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 200, 0.57).
narrative_ontology:measurement(impe_be_t250, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 250, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(impe_su_t50, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.63).
narrative_ontology:measurement(impe_su_t150, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 150, 0.67).
narrative_ontology:measurement(impe_su_t200, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(impe_su_t250, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 250, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint and imperial_mandate__loyalist_restoration_reading are sibling readings of a single contested kernel (imperial_mandate): the claim that Japanese imperial sovereignty is grounded in a divine or dynastic mandate. This story (bakufu_delegation_reading) treats the mandate's legitimacy-granting function as separable from active governance, authoring the shogunal delegation arrangement as a tangled_rope with moderate-to-substantial extraction (ε=0.58) captured chiefly by the shogunal house and samurai class. The sibling (loyalist_restoration_reading) treats mandate and governance as inseparable and would author the SAME historical arrangement as illegitimate usurpation with a different, likely higher, ε and a different victim structure (the emperor and loyalist factions as primary victims of the entire delegation apparatus, not merely excluded voices). The two stories are not two measurements of one constraint; they are two different constraints sharing a kernel, exactly per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
