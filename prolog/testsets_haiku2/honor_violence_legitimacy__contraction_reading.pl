% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefinition: Violence Exclusion (Contraction Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This constraint captures one reading of a contested kernel about honor
 *   and violence legitimacy. In the CONTRACTION READING, dueling became
 *   unthinkable not because it was made illegal or costly, but because the
 *   very concept of honor was redefined by state authority, legal
 *   institutions, and rising bourgeois professional classes to exclude
 *   violence as a legitimate response. The reading emphasizes conceptual
 *   shift: the space of what counts as honorable conduct contracted, and
 *   dueling fell outside it. This is distinct from the DROP READING (dueling
 *   remained legitimate but became rare due to practical costs) and the
 *   COMPOSITE READING (both mechanisms operated together). The constraint
 *   itself models the contraction reading's core claim: that honor
 *   redefinition was the primary mechanism, creating extractive pressure on
 *   those whose identity depended on the old definition while benefiting
 *   those whose honor derives from non-violent sources.
 *
 * KEY AGENTS:
 *   - State monopoly authority: redefines honor, enforces the exclusion through law and cultural authority
 *   - Aristocratic honor bearers: identity-locked to the old definition, facing extraction as it loses legitimacy
 *   - Bourgeois professionals: benefit from the redefinition, gaining honorable status without dueling
 *   - Dueling practitioners: organized resistance to the redefinition, progressively excluded from the legitimate honor conversation
 *   - Legal and educational institutions: operationalize the redefinition through courts, universities, and cultural production
 *   - Non-aristocratic groups: gain access to honor claims previously monopolized through combat readiness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.71).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefinition: Violence Exclusion (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'e0091c17-4644-4106-8dd3-03ebeb8b6b9f').
narrative_ontology:cs_kernel_codification('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', distributed).
narrative_ontology:cs_authority_grounding('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', extraction).
narrative_ontology:cs_interpretation_layer_present('e0091c17-4644-4106-8dd3-03ebeb8b6b9f').
narrative_ontology:cs_reading_relation('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', foundational, honor_definition_is_historically_contingent).
narrative_ontology:cs_axiom_status(honor_definition_is_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', honor_definition_is_historically_contingent, deontological).
narrative_ontology:cs_axiom('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', foundational, violence_is_incompatible_with_legitimate_honor).
narrative_ontology:cs_axiom_status(violence_is_incompatible_with_legitimate_honor, holdable).
narrative_ontology:cs_axiom_grounding('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', violence_is_incompatible_with_legitimate_honor, deontological).
narrative_ontology:cs_reference_frame('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', dueling_as_legitimate_honor).
narrative_ontology:cs_drift_state('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', post_redefinition_consolidation_1850, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('e0091c17-4644-4106-8dd3-03ebeb8b6b9f', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_monopoly_authority).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, non_aristocratic_groups).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_honor_bearers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, dueling_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professionals).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, women_excluded_from_dueling).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_of_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, civic_equality_incompatible_with_honor_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Redefines what counts as legitimate honor response through law, education, and cultural authority. Enforces the exclusion of violence from honor by prosecuting duelists, refusing to recognize dueling debts, and gradually institutionalizing honor through non-violent channels (courts, guilds, credentials, published precedent). Collects legitimacy from this redefinition—the state becomes the sole arbiter of legitimate honor claims.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_monopoly_authority, agenda_setter,
    institutional, generational, analytical, national).

% Had organized their entire social position around violent honor response (dueling) as the sole mechanism for defending reputation and status. As honor itself is redefined to exclude violence, they face a dilemma: comply with the new definition and lose the traditional anchor of aristocratic distinction, or continue dueling and accept criminal prosecution. Their identity as honor-bearers is bound to the old definition; the redefinition attacks the structural basis of their social role.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_honor_bearers, payer,
    powerful, generational, identity_locked, national).

% Gain access to honor claims and reputation mechanisms previously monopolized by aristocrats through violent readiness. As honor is redefined to exclude violence, it becomes available through literacy, education, professional achievement, and economic success—channels where non-aristocratic groups can compete on equal terms. They are freed from the requirement to be dueling-ready to defend reputation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, non_aristocratic_groups, beneficiary,
    moderate, generational, mobile, national).

% Active practitioners of dueling across Europe, organized by regiment, academy, and family tradition. As the cultural definition of honor shifts, their practice becomes incomprehensible to the rising generation—not because dueling becomes illegal (that is enforcement), but because the conceptual framework that made dueling a legitimate honor response dissolves. They are progressively excluded from the conversation about what honor means.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_practitioners, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, dueling_practitioners, excluded).

% Ascendant urban professionals (lawyers, doctors, merchants, bureaucrats) whose honor derives from credential, expertise, and reputation for integrity rather than combat readiness. The redefinition of honor to exclude violence legitimates their social position and authority without requiring them to participate in dueling culture. They become the model of honorable conduct.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professionals, beneficiary,
    powerful, generational, mobile, national).

% Courts, legislatures, and legal commentators operationalize the redefinition by refusing to recognize dueling as legitimate self-defense, by prosecuting participants, and by authoring new legal concepts of reputation and defamation that channel honor disputes into courtrooms. Law becomes the institutional form through which honor redefinition is enforced.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_authority_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Universities, publishing houses, theaters, and salons propagate the new definition of honor through pedagogy, literature, and social conversation. They teach that honor is compatible with reason, commerce, and peaceful resolution of disputes. They ridicule dueling through satire and moral critique. They author the cultural meaning of what honorable conduct looks like.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, education_cultural_institutions, agenda_setter,
    organized, generational, analytical, national).

% Structurally excluded from dueling as an honor mechanism (dueling was male-exclusive), but also excluded from the honor system it sustained. As honor is redefined to detach from violence, women's exclusion from the old mechanism persists but the rationale shifts—they are now excluded not because they cannot fight but because the new honor system has different gatekeeping logics (credential, education, economic role). Their situation is complex: the redefinition opens some channels while maintaining their marginality through new mechanisms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_excluded_from_dueling, beneficiary,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, women_excluded_from_dueling, excluded).

% Rural populations had little participation in dueling culture (that was an urban/aristocratic/military phenomenon) but are subject to the state's legal enforcement of the redefinition and its suppression of honor-violence as a dispute mechanism. They would have alternative local mechanisms for reputation and honor, but these too are overwritten by the state monopoly on legitimacy.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, village_and_rural_populations, excluded,
    powerless, biographical, trapped, regional).

% Analyze the constraint from outside the historical societies that experienced it. They map the shift in conceptual definitions of honor, trace the mechanism of redefinition through institutional channels, and measure the constraint's extractive and suppressive dimensions.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, observer_historians_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, state_monopoly_authority).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from a violence-dependent honor system to a violence-independent system by redefining what counts as legitimate honor response. Solves the problem of how to maintain reputation defense and personal dignity in complex urban societies without requiring combat readiness. Establishes new mechanisms (law, credential, education, economic role) through which honor can be claimed and defended.
% TRANSFER_FUNCTION: Transfers the legitimacy surplus from dueling practitioners and aristocratic honor-culture to state legal authority, bourgeois professionals, and educational institutions. Moves from honor-through-readiness-for-violence to honor-through-expertise, credential, and institutional position. Extracts from those whose identity depended on the old definition; delivers to those whose honor derives from the new institutions.
% ABSENT_VOICES: The rising generation of aristocrats (socialized into the new definition before they could internalize the old one) are excluded from defending the old framework because they never learned it. Rural populations and those outside urban professional networks are excluded from the conversation about what honor means, even though they are subject to the state's enforcement of the new definition. Women, systematically excluded from dueling but also from the redefinition process, are absent from the official conversation about what their honor should consist of.
% DISAPPEARANCE_RATIONALE: If the honor redefinition vanished overnight and dueling suddenly reverted to legitimacy, the institutional apparatus built on non-violent honor (courts, credentials, professional standards) would lose its legitimacy foundation. Social position could no longer be safely claimed through education or expertise alone; those with identity investment would resume dueling to defend reputation. The state's monopoly on legitimacy would be directly challenged.
% FOUNDING_PROBLEM: Societies needed a reputation defense mechanism that (1) scaled beyond dyadic combat, (2) included non-military populations in honorable status claims, and (3) operated through institutions that could be centrally monitored. The dueling system failed all three criteria at societal scale.
% FOUNDING_PROBLEM_CORROBORATION: State legal authorities and bourgeois professionals attest the founding problem drove the redefinition. Military historians document that standing armies made individual dueling incompatible with command authority. Sociologists of the period (Weber, later analysts) corroborate that institutional professionalization required honor redefinition. Dueling-culture defenders (surviving testimonies) attest the problem was never solved and dueling remained necessary for true honor. Outside corroboration comes from comparative historical analysis showing that societies without honor redefinition experienced persistent dueling into the 20th century, and that honor-definition changes correlate with state consolidation.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by 1850 because the redefinition transfers the legitimacy surplus from dueling practitioners to state authority and institutional mediators—those who control the definition of honor now extract recognition and status from those whose old position depended on the old definition. Suppression is high (0.71) because the redefinition operates through enforcement: dueling becomes not merely discouraged but incomprehensible, prosecuted, and eventually structurally unthinkable. The suppression is active because the old definition does not disappear naturally—it must be actively overwritten through law, education, and cultural authority. Theater rises (0.42) as the constraint matures because much of the institutional apparatus maintains a narrative about honor, reputation, and justice that obscures the extractive transfer: the state presents itself as liberating people from the violence of dueling, while actually consolidating its monopoly on legitimacy. Accessibility collapse reaches 0.79 because, by 1850, the old definition of honor-through-dueling has been so thoroughly excluded from legitimate discourse that the alternative (non-violent honor response) is nearly the only thinkable option—the conceptual space itself has contracted. Resistance starts high (0.62 at the structural level in 1650) because dueling was deeply entrenched in aristocratic and military organization, but declines (0.28 by 1850) as the redefinition succeeds and the old framework becomes literally incomprehensible to the rising generation. The grid captures the uneven pace of contraction across social levels: aristocratic organizational resistance is strong early and collapses by 1850; individual-level resistance persists longer because identity-locked individuals maintain the old frame even as the structural legitimacy collapses around them.
 *
 * PERSPECTIVAL GAP:
 *   From the state's institutional seat, the redefinition is a civilizational achievement—the creation of a system that scales reputation defense without requiring violence, that includes more people in honor participation, and that consolidates legitimate authority. From the aristocratic dueling practitioner's seat, the same process is an extraction of their distinctive status, a delegitimization of their identity, and a hostile takeover of the honor system they maintained. From the non-aristocratic beneficiary seat, it is liberation—access to honorable status without requiring combat readiness. The engine computes these divergences from the structural data: the aristocratic seat faces high directionality (d near 1.0 = target), the state faces low directionality (d near 0.0 = beneficiary), and the beneficiary non-aristocratic seat faces moderate directionality (d near 0.5 = mixed). The same redefinition produces different types when computed from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: state monopoly authority (collects legitimacy and control), bourgeois professionals (gain honorable status), non-aristocratic groups (access to honor without combat). Their directionality is low (near beneficiary end): the constraint subsidizes their position. Victims: aristocratic honor bearers (identity-locked, exit options collapse as the old definition loses legitimacy), dueling practitioners (organized but progressively excluded). Their directionality is high (near target end): the constraint extracts their distinctive status. The identity-lock is crucial for aristocratic directionality: they cannot exit dueling culture without ceasing to be aristocratic in the traditional sense; their exit_options are classified as identity_locked, which keeps d high despite their power (powerful agents with identity_locked exit face full extraction). Dueling practitioners face similar identity-locking but are organized rather than institutional in power, which affects how the constraint's enforcement machinery targets them differently than isolated identity-locked individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the constraint's own operation: a reputation defense system that required combat readiness and violent confrontation did not scale to complex urban societies. But by 1850, dueling has become rare not because the founding problem is solved (people still need reputation defense) but because the state has monopolized the definition of legitimate honor response. The founding_problem_status is contested because: (1) state authorities claim the founding problem is solved (we now have a non-violent honor system), (2) dueling defenders (where testimony survives) claim the problem persists (reputation still requires courage and willingness to defend), and (3) observers can argue whether the original problem was solved or simply redefined away. The constraint avoids mandatrophy because the redefinition remains actively maintained: the state continuously enforces the new definition through law, education, and cultural authority. If enforcement stopped, dueling would likely resume among those with identity investment. However, the rising generation, educated in the new definition, would view dueling as incomprehensible—a form of madness rather than legitimate conduct. This creates a path-dependent mandatrophy risk: if the state's enforcement apparatus weakens, the constraint might persist through internalized redefinition (the new definition becomes self-maintaining once absorbed through education) or might collapse suddenly if a new political regime explicitly reverses the definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_mechanism,
    'Was dueling abandoned primarily because the concept of honor was redefined to exclude violence (contraction), or primarily because the external costs of dueling increased (drop: punishment, loss of military position, legal jeopardy)?',
    'Textual and institutional analysis: examine how legal authorities, educators, and cultural figures described honor and dueling over the interval. Did they emphasize that dueling is now incompatible with honor (redefinition/contraction) or that dueling carries legal consequences (cost/drop)? Trace the genealogy of anti-dueling arguments to distinguish normative redefinition from external cost imposition. Examine whether dueling persists in contexts where external costs are low (e.g., remote areas, military cultures, private clubs); persistence despite low costs would suggest contraction is the primary mechanism.',
    'If contraction is primary, the constraint is a case of ideology-driven transformation where legitimacy itself is rewritten. The engine would classify it as more tangled_rope (coordination through redefinition + extraction from those displaced) or even snare (pure extraction masked by redefinition). If drop is primary, the constraint is more purely suppressive enforcement. If both operated together (composite reading), the classification would blend elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_mechanism, empirical, 'Whether honor redefinition (contraction) or external costs (drop) is the primary mechanism of dueling''s decline.').

omega_variable(
    identity_lock_internalization_boundary,
    'For aristocratic honor bearers, did the redefinition of honor operate as external suppression (state authority preventing them from maintaining the old definition) or as internalized identity-dissolution (they came to accept that their old definition was illegitimate)?',
    'Biographical and memoir evidence: trace accounts from aristocrats across the interval. Did they experience the redefinition as imposed enforcement (external suppression) or as a loss of confidence in their own framework (internalized collapse)? Did they resist actively or did their resistance fade as their children accepted the new definition? Intergenerational pattern analysis: do later-generation aristocrats attempt to defend dueling or do they accept the redefinition as correct?',
    'If primarily suppression, the constraint''s suppression score of 0.71 accurately reflects active enforcement machinery. If primarily internalized, the suppression metric understates the constraint''s power—internalized suppression (the target believes the constraint is legitimate) is more difficult to overcome and less likely to produce resistance. The identity_locked exit_options classification assumes some suppression is internalized; this omega probes the balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization_boundary, empirical, 'Whether suppression of the old honor definition is primarily external (enforced by authority) or internalized (the targets come to accept it as legitimate).').

omega_variable(
    redefinition_necessity_ambiguity,
    'Did the redefinition of honor to exclude violence solve a genuine coordination problem, or was it primarily a mechanism for state consolidation of authority (extraction framed as coordination)?',
    'Comparative institutional analysis: examine societies that failed to achieve honor redefinition (where dueling persisted longer). Did they experience worse outcomes (more violence, less social cohesion, state weakness) or merely different outcomes? Did non-dueling societies actually need the redefinition to achieve complex urban organization, or was the organizational form achievable without it? Historical counterfactual: if dueling had remained legitimate but become rare through technological/organizational change (standing armies replacing individual armed aristocrats), would complex societies have developed without honor redefinition?',
    'If the redefinition solved a genuine coordination problem, the constraint is truly tangled_rope (coordination + asymmetric extraction). If it was primarily extractive—state monopolization of legitimacy framed as coordination—the constraint is closer to snare. The distinction affects long-term sustainability and the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redefinition_necessity_ambiguity, conceptual, 'Whether honor redefinition was necessary coordination or disguised state extraction.').

omega_variable(
    reading_foreclosure_question,
    'Does the contraction reading (honor is redefined to exclude violence) logically foreclose the drop reading (dueling became rare due to external costs), or can both readings coexist as different framings of the same events?',
    'Logical analysis: if the contraction reading is true (honor is redefined), does that make it impossible for the drop reading to also be true? Or are they compatible: dueling is both redefined out of legitimacy AND made costly through enforcement? The question is whether the readings offer different descriptions of the same constraint or describe genuinely different constraints (per the ε-invariance principle, OQ-258). If they describe the same constraint differently, they coexist; if they describe different constraints, they form a constraint family requiring separate JSON files.',
    'This omega tests the kernel decomposition boundary. If foreclosure holds, one reading is architecturally impossible given the other''s core claim. If coexistence holds, both readings remain live and feed different institutional positions. The resolution affects how the corpus treats the three sibling readings and whether they are siblings (different readings of one kernel) or a family of distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Whether the contraction and drop readings are compatible descriptions or logically exclusive claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_violence_legitimacy__contraction_reading, theater_ratio, 1650, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1650, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.38).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1825, honor_violence_legitimacy__contraction_reading, theater_ratio, 1825, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1825, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1650, 0.35).
narrative_ontology:measurement_basis(hono_be_t1650, projected).
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.61).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1825, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1825, 0.68).
narrative_ontology:measurement_basis(hono_be_t1825, observed).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.68).
narrative_ontology:measurement_basis(hono_be_t1850, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1650, 0.28).
narrative_ontology:measurement_basis(hono_su_t1650, projected).
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.42).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.56).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.66).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1825, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1825, 0.71).
narrative_ontology:measurement_basis(hono_su_t1825, observed).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.71).
narrative_ontology:measurement_basis(hono_su_t1850, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1650, tn=1850
narrative_ontology:measurement(hono_grid_01, honor_violence_legitimacy__contraction_reading, accessibility_collapse(class), 1650, 0.12).
narrative_ontology:measurement(hono_grid_02, honor_violence_legitimacy__contraction_reading, accessibility_collapse(class), 1850, 0.76).
narrative_ontology:measurement(hono_grid_03, honor_violence_legitimacy__contraction_reading, accessibility_collapse(individual), 1650, 0.08).
narrative_ontology:measurement(hono_grid_04, honor_violence_legitimacy__contraction_reading, accessibility_collapse(individual), 1850, 0.81).
narrative_ontology:measurement(hono_grid_05, honor_violence_legitimacy__contraction_reading, accessibility_collapse(organizational), 1650, 0.18).
narrative_ontology:measurement(hono_grid_06, honor_violence_legitimacy__contraction_reading, accessibility_collapse(organizational), 1850, 0.82).
narrative_ontology:measurement(hono_grid_07, honor_violence_legitimacy__contraction_reading, accessibility_collapse(structural), 1650, 0.22).
narrative_ontology:measurement(hono_grid_08, honor_violence_legitimacy__contraction_reading, accessibility_collapse(structural), 1850, 0.79).
narrative_ontology:measurement(hono_grid_09, honor_violence_legitimacy__contraction_reading, resistance(class), 1650, 0.55).
narrative_ontology:measurement(hono_grid_10, honor_violence_legitimacy__contraction_reading, resistance(class), 1850, 0.35).
narrative_ontology:measurement(hono_grid_11, honor_violence_legitimacy__contraction_reading, resistance(individual), 1650, 0.48).
narrative_ontology:measurement(hono_grid_12, honor_violence_legitimacy__contraction_reading, resistance(individual), 1850, 0.38).
narrative_ontology:measurement(hono_grid_13, honor_violence_legitimacy__contraction_reading, resistance(organizational), 1650, 0.68).
narrative_ontology:measurement(hono_grid_14, honor_violence_legitimacy__contraction_reading, resistance(organizational), 1850, 0.22).
narrative_ontology:measurement(hono_grid_15, honor_violence_legitimacy__contraction_reading, resistance(structural), 1650, 0.62).
narrative_ontology:measurement(hono_grid_16, honor_violence_legitimacy__contraction_reading, resistance(structural), 1850, 0.28).
narrative_ontology:measurement(hono_grid_17, honor_violence_legitimacy__contraction_reading, stakes_inflation(class), 1650, 0.35).
narrative_ontology:measurement(hono_grid_18, honor_violence_legitimacy__contraction_reading, stakes_inflation(class), 1850, 0.68).
narrative_ontology:measurement(hono_grid_19, honor_violence_legitimacy__contraction_reading, stakes_inflation(individual), 1650, 0.42).
narrative_ontology:measurement(hono_grid_20, honor_violence_legitimacy__contraction_reading, stakes_inflation(individual), 1850, 0.72).
narrative_ontology:measurement(hono_grid_21, honor_violence_legitimacy__contraction_reading, stakes_inflation(organizational), 1650, 0.28).
narrative_ontology:measurement(hono_grid_22, honor_violence_legitimacy__contraction_reading, stakes_inflation(organizational), 1850, 0.81).
narrative_ontology:measurement(hono_grid_23, honor_violence_legitimacy__contraction_reading, stakes_inflation(structural), 1650, 0.15).
narrative_ontology:measurement(hono_grid_24, honor_violence_legitimacy__contraction_reading, stakes_inflation(structural), 1850, 0.74).
narrative_ontology:measurement(hono_grid_25, honor_violence_legitimacy__contraction_reading, suppression(class), 1650, 0.12).
narrative_ontology:measurement(hono_grid_26, honor_violence_legitimacy__contraction_reading, suppression(class), 1850, 0.68).
narrative_ontology:measurement(hono_grid_27, honor_violence_legitimacy__contraction_reading, suppression(individual), 1650, 0.08).
narrative_ontology:measurement(hono_grid_28, honor_violence_legitimacy__contraction_reading, suppression(individual), 1850, 0.71).
narrative_ontology:measurement(hono_grid_29, honor_violence_legitimacy__contraction_reading, suppression(organizational), 1650, 0.22).
narrative_ontology:measurement(hono_grid_30, honor_violence_legitimacy__contraction_reading, suppression(organizational), 1850, 0.82).
narrative_ontology:measurement(hono_grid_31, honor_violence_legitimacy__contraction_reading, suppression(structural), 1650, 0.18).
narrative_ontology:measurement(hono_grid_32, honor_violence_legitimacy__contraction_reading, suppression(structural), 1850, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'honor_violence_legitimacy.' The contraction_reading emphasizes honor's conceptual redefinition as the primary mechanism. The drop_reading (in a separate JSON file) emphasizes external costs and practical rareness. The composite_reading (in a separate JSON file) models both mechanisms together. The three stories form a constraint family linked by network.affects_constraints. Each story has its own ε, its own beneficiary/victim structure, and its own cs_structure reading_relations. The upstream reading (contraction) influences the downstream readings by establishing that honor redefinition is possible and was historically attempted; the downstream readings offer alternative framings of the same historical phenomenon. Per the ε-invariance principle, each reading instantiates a different constraint with potentially different ε values (drop reading has lower ε if external costs alone are emphasized; composite reading blends them).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
