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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor Settlement Legitimacy (Composite Reading: Multiple Reinforcing Decline Mechanisms)
 *   domain: legal/cultural/historical
 *
 * SUMMARY:
 *   Dueling's decline in Europe (roughly 1700–1850) was not caused by a
 *   single mechanism but by the convergence of multiple reinforcing pathways:
 *   cultural redefinition of honor (contraction: honor became 'unthinkable'
 *   to practice through dueling), institutional consolidation of state legal
 *   monopoly (material suppression), professionalization of law (displacement
 *   of dispute settlement from ritual to credentialed expertise), moral
 *   reform narratives (reframing dueling as barbaric), and commercial
 *   interests in predictable legal systems. This composite reading asserts
 *   that all mechanisms operated simultaneously and that each would
 *   independently suppress dueling, but their convergence accelerated and
 *   entrenched the decline. The constraint is not dueling itself but the
 *   legitimacy structure that made dueling a viable dispute-settlement and
 *   status-maintenance mechanism. As that legitimacy collapsed through
 *   multiple pathways, the practice became not just illegal but unthinkable —
 *   not because any single mechanism made it so, but because the combination
 *   rendered it both legally impossible, institutionally obsolete, culturally
 *   shameful, and materially unrewarding. The reading asserts that the
 *   decline is overdetermined: any one or two of the mechanisms would have
 *   sufficed, but all three together ensured near-total suppression.
 *
 * KEY AGENTS:
 *   - nationalist_state_apparatus: institutional agenda-setter with monopoly on legitimate violence, benefits from elimination of rival dispute-settlement mechanisms
 *   - bourgeois_legal_professionals: organized beneficiaries who consolidate professional monopoly over dispute resolution as dueling declines
 *   - civil_society_reformers: organized beneficiaries who gain authority through moral reframing of honor as incompatible with civilization
 *   - honor_culture_adherents: powerful targets with identity-locked exit, face costs of criminalization and cultural stigmatization
 *   - aristocratic_traditional_elites: powerful targets with constrained exit, lose authority claim grounded in honor code as legal and cultural frameworks shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.72).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor Settlement Legitimacy (Composite Reading: Multiple Reinforcing Decline Mechanisms)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "legal/cultural/historical").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, 'dd031680-8caa-4966-8fd6-f68700c99039').
narrative_ontology:cs_kernel_codification('dd031680-8caa-4966-8fd6-f68700c99039', distributed).
narrative_ontology:cs_authority_grounding('dd031680-8caa-4966-8fd6-f68700c99039', extraction).
narrative_ontology:cs_interpretation_layer_present('dd031680-8caa-4966-8fd6-f68700c99039').
narrative_ontology:cs_reading_relation('dd031680-8caa-4966-8fd6-f68700c99039', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd031680-8caa-4966-8fd6-f68700c99039', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('dd031680-8caa-4966-8fd6-f68700c99039', foundational, multiple_mechanisms_necessary_for_decline).
narrative_ontology:cs_axiom_status(multiple_mechanisms_necessary_for_decline, holdable).
narrative_ontology:cs_axiom_grounding('dd031680-8caa-4966-8fd6-f68700c99039', multiple_mechanisms_necessary_for_decline, empirically_contingent).
narrative_ontology:cs_axiom('dd031680-8caa-4966-8fd6-f68700c99039', secondary, cultural_unthinkability_insufficient_alone).
narrative_ontology:cs_axiom_status(cultural_unthinkability_insufficient_alone, holdable).
narrative_ontology:cs_axiom_grounding('dd031680-8caa-4966-8fd6-f68700c99039', cultural_unthinkability_insufficient_alone, empirically_contingent).
narrative_ontology:cs_reference_frame('dd031680-8caa-4966-8fd6-f68700c99039', aristocratic_honor_code_legitimacy).
narrative_ontology:cs_drift_state('dd031680-8caa-4966-8fd6-f68700c99039', bourgeois_legal_professionalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd031680-8caa-4966-8fd6-f68700c99039', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, nationalist_state_apparatus).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_legal_professionals).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, civil_society_reformers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, aristocratic_traditional_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, commercial_middle_classes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes legitimate violence and dispute resolution through state courts. Criminalizes dueling via statute and enforces prohibition through police, prosecution, and judicial punishment. Frames the suppression as rationalization and modernization of the legal system. Benefits from elimination of rival dispute-settlement mechanisms that contest state authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, nationalist_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Consolidate professional monopoly over dispute resolution and legal interpretation as dueling declines. Their status rises as the only legitimate arbiters of honor and right. They benefit from the shift of dispute-settlement authority from personal/ritualized mechanisms to codified law administered by the profession.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_legal_professionals, beneficiary,
    organized, generational, arbitrage, national).

% Mobilize moral reform narratives against dueling on grounds of its violence, irrationality, and threat to social order. Frame dueling as backwards, barbaric, and incompatible with civilization. Gain cultural authority and institutional positions as guardians of enlightenment values. Benefit from the reframing of honor from aristocratic code to bourgeois respectability.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, civil_society_reformers, beneficiary,
    organized, biographical, mobile, national).

% For whom honor-defense through dueling is foundational to identity, social standing, and claim to legitimacy. As the state criminalizes the practice, as legal professionals monopolize dispute resolution, and as cultural narratives redefine honor as incompatible with civilization, adherents face costs: criminal prosecution, social ostracism, loss of status claims, and erosion of the entire identity structure that sustained their power.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_culture_adherents, payer,
    powerful, biographical, identity_locked, national).

% Historically relied on dueling as a dispute-settlement mechanism and status marker. As dueling is criminalized and redefined as barbaric, their claim to authority grounded in honor code erodes. They are increasingly excluded from meaningful participation in the mechanisms (courts, professional bodies, civil society) through which disputes are now legitimately settled. Their exit options narrow: adapt to bourgeois legal and social norms or face marginalization.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_traditional_elites, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, aristocratic_traditional_elites, excluded).

% Benefit from a unified, predictable, codified legal system that replaces the unpredictability and violence of honor-based dispute settlement. Their commercial interests are served by state monopoly on legitimate violence and by courts that enforce contracts via law rather than ritual combat.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, commercial_middle_classes, beneficiary,
    organized, generational, mobile, national).

% Official church teaching condemns dueling as sinful and contrary to Christian doctrine on the sanctity of life. Church institutions lend moral authority to the state's criminalization and to reformers' condemnation. However, the church's own authority over morality is being displaced by secular legal authority and bourgeois civil society.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, clergy_and_institutional_religion, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, nationalist_state_apparatus).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling as a constraint coordinates dispute settlement and status defense within honor culture: a gentleman who refuses a duel or accepts insult without satisfaction loses standing, which affects credit, marriage prospects, and political influence. The practice solves a real coordination problem: how to maintain honor and trust networks when formal law is weak, distant, or seen as incompetent to judge matters of reputation. It also coordinates a shared framework for what counts as a legitimate claim to authority and rank.
% TRANSFER_FUNCTION: Dueling transfers risk of death and injury from disputants to themselves (as both parties accept lethal hazard), but also transfers authority over dispute settlement from state institutions to honor culture community. It transfers status claims from legal professionals and state arbiters to those proven brave in combat. The constraint also transfers cultural legitimacy: as dueling declines, authority and legitimacy flow from the honor code to the state legal system, from aristocratic ritual to bourgeois law.
% ABSENT_VOICES: Ordinary laborers and lower classes whose disputes were never settled by dueling code (that mechanism was reserved for gentlemen/aristocrats). Women, whose honor was defended by male kin through dueling, had no direct role in the mechanism. Colonial subjects and non-European peoples whose honor codes differed from European aristocratic dueling were excluded from legitimacy in the European frame. Their absence is structural: dueling legitimacy never extended to them, so their voices opposing the practice were not interior to the dispute over its decline.
% DISAPPEARANCE_RATIONALE: If dueling legitimacy disappeared overnight (which is what this reading describes as actually happening), honor-culture identity loses its material practice and ritual substrate. Aristocratic elites lose a primary mechanism for maintaining status and settling disputes among themselves. The state gains uncontested monopoly on legitimate violence and dispute settlement. Commercial and bourgeois actors consolidate control over wealth and authority through legal institutions. The entire structure of what counts as legitimate honor, authority, and social rank would reorganize around state law and bourgeois respectability rather than martial ritual.
% FOUNDING_PROBLEM: In honor-culture societies (medieval through early modern Europe), disputes over insult, property, and rank could not be reliably settled by weak or distant state institutions. Aristocratic honor required public vindication and peer recognition. Dueling as a ritual mechanism resolved the coordination problem: a challenge, formal rules, and lethal outcome that determined winner and loser, restoring or confirming status. It also served as a mechanism of constraint-reinforcement: refusing to duel meant accepting dishonor, so the ritual maintained itself through identity fusion.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship and contemporary legal codes confirm that the founding coordination problem (weak state institutions, need for honor vindication) is now dead: state courts are robust, national monopoly on violence is established, and legal mechanisms for contract enforcement and reputation management exist. Aristocratic elites themselves attest (through their historical retreat from dueling, even when state enforcement was weak) that the problem the practice solved no longer exists in their lived experience. Outside beneficiaries attest that the problem was never real — that honor culture was always a mask for elite power maintenance, not a solution to coordination failure.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end because the constraint transfers authority over dispute settlement from honor culture to state/professional mechanisms and imposes costs (legal jeopardy, identity crisis, status loss) on adherents. The measurement series shows a cumulative rise: early stages have moderate extraction (honor codes still partially legitimate, enforcement sporadic) but as all three mechanisms converge (cultural unthinkability spreads, legal prohibition tightens, professional monopoly consolidates), extraction rises. Suppression is high (0.72) because the constraint depends on active enforcement: without state police and courts actively prosecuting duelers, without cultural reformers actively stigmatizing the practice, without professional bodies actively displacing dispute resolution authority, honor-culture adherents would continue the practice. Theater ratio rises over time (0.18 to 0.41) because enforcement increasingly consists of performative suppression (legal statutes that are rarely prosecuted, cultural condemnation of a practice that is already rare) rather than actual functional conflict. The accessibility of alternatives (dueling as a legitimate choice) collapses almost completely by the end (0.79) because the practice has become illegal, stigmatized, institutionally impossible, and culturally incomprehensible to elite youth. Resistance is moderate (0.58) because some honor-culture adherents continue practicing dueling illegally or defend it theoretically even as the practice fades; they mount real resistance but lack institutional power to reverse the decline.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, the constraint is a rational monopolization of dispute settlement and elimination of rival violence (functional necessity). From legal professionals' perspective, it is the professionalization and codification of legitimate authority (progress). From civil reformers' perspective, it is the suppression of barbarism and irrationality (enlightenment). From honor-culture adherents' perspective, it is the criminalization and cultural delegitimization of their identity and status system (extraction and erosion). From aristocratic elites' perspective, it is the displacement of their authority from martial prowess to legal-professional credentials (loss of legitimacy claim). The engine computes each seat's type from structural data: the state and legal professionals sit at the beneficiary end (low d, negative χ if they experience subsidy effects), while honor-culture adherents and aristocratic elites sit at the target end (high d, high χ). This divergence is the engine's measurement; the JSON claim of 'tangled_rope' asserts that both genuine coordination (monopoly on violence, unified legal system) and asymmetric extraction (suppression of honor code, consolidation of professional power) are present.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and legal professionals benefit from the consolidation of dispute-settlement authority; they are listed as beneficiaries because the constraint's operation transfers rents to them (professional monopoly, state authority). Honor-culture adherents and aristocratic elites are victims because the constraint imposes costs (criminalization, status loss, identity dissolution) with limited exit options (identity-locked or constrained by the very institutional changes that suppress dueling). The constraint requires active enforcement (state prosecution, cultural condemnation, professional gatekeeping) to persist — without all three mechanisms, honor-culture communities would revert to dueling. This makes the constraint tangled_rope: it coordinates a unified legal system (genuine coordination function) while simultaneously extracting authority and rents from those whose legitimacy derives from honor code (asymmetric extraction from identity-locked targets).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (weak state institutions, need for honor vindication in honor-culture societies) is dead: state legal institutions are robust and universally legitimate; honor codes are no longer the mechanism for status maintenance among elites. As the founding problem died, the constraint should have dissolved. Instead, it persists as a codified legal prohibition and cultural norm long after the practice has faded naturally. This is a mandatrophy signature: the constraint that was built to solve a real coordination problem now persists as legal theater and cultural condemnation of a practice that has already become culturally unthinkable for independent reasons (the contraction mechanism). The theater ratio rising over time (0.18 to 0.41) captures this mandatrophy: enforcement becomes increasingly performative (statutes that rarely need to be invoked, public condemnation of an already-rare practice) rather than functional (actually preventing dueling through police action). The constraint is a tangled rope that has partially decomposed into scaffolding and piton: the rope's coordination function (unified legal system) remains real, but the rope's enforcement overhead (suppressing residual honor-culture practice) is increasingly theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_basis_coherence,
    'The measurement series shows suppression_requirement rising steeply (0.35 to 0.72) even as practice fades, consistent with mandatrophy. But does the steepness of rise reflect actual changes in enforcement intensity (increased police activity, legal penalties) or changes in what was needed to maintain the prohibition (shifting from suppression of widespread practice to prevention of rare fringe cases)?',
    'Historical records of prosecution rates, legal penalties, state expenditure on suppression, and enforcement capacity over time. If suppression intensity rose but practice was already fading naturally, the rise reflects theater; if suppression intensity rose in response to continued widespread practice, it reflects functional enforcement.',
    'If theater dominates, the mandatrophy reading is correct and the constraint is degrading into piton. If suppression responded to persistent practice threat, the tangled rope classification is more robust and the constraint is genuinely enforced rather than theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_basis_coherence, empirical, 'Whether rising suppression_requirement reflects increasing enforcement intensity or increasing theater as practice fades.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__composite_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__composite_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__composite_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__composite_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__composite_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__composite_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hono_su_t10, honor_settlement_legitimacy__composite_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__composite_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__composite_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel decomposes into three readings of the same constraint (the legitimacy structure that sustained dueling dispute settlement): composite_reading (this file) asserts multiple reinforcing mechanisms converge; contraction_reading isolates cultural unthinkability as the dominant driver; drop_reading asserts practice persisted in fringe honor cultures. All three readings share the same referent (honor settlement legitimacy as a structural constraint) but differ in what mechanisms they emphasize and what causal weight they assign. The composite_reading influences both siblings by providing a framework within which their emphases can be situated (culture is one mechanism, but not the only one; persistence occurred, but was fringe and accelerating toward extinction). Generate each reading as a separate constraint story with its own ε-invariance frame and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
