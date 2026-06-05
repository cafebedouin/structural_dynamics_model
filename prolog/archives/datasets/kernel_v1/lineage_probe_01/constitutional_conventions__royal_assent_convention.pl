% ============================================================================
% CONSTRAINT STORY: constitutional_conventions__royal_assent_convention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_conventions__royal_assent_convention, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_conventions__royal_assent_convention
 *   human_readable: Royal Assent Convention: The Unused Veto (1708–Present)
 *   domain: constitutional_law/political_convention
 *
 * SUMMARY:
 *   The royal assent convention constrains the Crown's formal legislative
 *   veto power through an unwritten rule enforced by political sanction: if a
 *   reigning monarch refuses assent to a bill duly passed by Parliament, the
 *   consequence is immediate constitutional crisis and forced abdication. No
 *   act of law terminates the power; the power is structurally alive but
 *   functionally dead — suppressed entirely through the enforcement of the
 *   convention. This constraint is ONE READING of a contested kernel: the
 *   British constitutional system grounds its legitimacy in several
 *   stabilized commitments (collective Cabinet responsibility, ministerial
 *   accountability, parliamentary mandate deference, and royal legislative
 *   restraint). This story instantiates the royal_assent_convention reading,
 *   which claims that the monarch's legislative will is subordinate to
 *   parliamentary finality and that this subordination must remain unspoken —
 *   the fiction is that assent is voluntary, not coerced. The sibling
 *   readings operate the same constitutional system but instantiate different
 *   commitments: collective_responsibility governs how the Cabinet speaks to
 *   Parliament, ministerial_responsibility governs bureaucratic
 *   accountability, salisbury_convention governs how the Lords defer to
 *   manifesto legislation. All four readings occupy the same formal
 *   constitutional structure; all four are defended by major political
 *   actors; none is formally abolished; all depend on the unwritten rule that
 *   they must never be formalized (formalization would break the fiction that
 *   they are voluntary).
 *
 * KEY AGENTS:
 *   - Parliament: Primary beneficiary (institutional/arbitrage) — gains legislative finality and protection against Crown override; experiences constraint as pure coordination (Rope perspective)
 *   - Reigning Monarch: Primary victim (powerless/trapped) — legislative will structurally suppressed; cannot exercise formal power without triggering abdication; experiences constraint as pure extraction (Snare perspective)
 *   - The Monarchy as Institution: Secondary beneficiary (powerful/constrained) — survives in democratic age by accepting political neutrality; experiences constraint as mixed coordination-extraction (Tangled Rope perspective)
 *   - The Crown in Historical Form: Victim of gradualized nullification (institutional/arbitrage) — legislative power slowly eroded from living possibility (1708) to theatrical fiction (2024); experiences constraint as inert performance (Piton perspective)
 *   - Constitutional Reform Movements: Organized agents (organized/mobile) — treat convention as temporary improvisation awaiting formal codification; see constraint as transitional (Scaffold perspective)
 *   - The Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as constitutional law; threatened to commit false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_conventions__royal_assent_convention, 0.08).
domain_priors:suppression_score(constitutional_conventions__royal_assent_convention, 0.72).
domain_priors:theater_ratio(constitutional_conventions__royal_assent_convention, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_conventions__royal_assent_convention, extractiveness, 0.08).
narrative_ontology:constraint_metric(constitutional_conventions__royal_assent_convention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_conventions__royal_assent_convention, theater_ratio, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_conventions__royal_assent_convention, piton).
narrative_ontology:human_readable(constitutional_conventions__royal_assent_convention, "Royal Assent Convention: The Unused Veto (1708–Present)").
narrative_ontology:topic_domain(constitutional_conventions__royal_assent_convention, "constitutional_law/political_convention").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_conventions__royal_assent_convention, '8316fa50-11ef-43b0-82d8-74614607a2df').
narrative_ontology:cs_kernel_codification('8316fa50-11ef-43b0-82d8-74614607a2df', fixed_text).
narrative_ontology:cs_authority_grounding('8316fa50-11ef-43b0-82d8-74614607a2df', lineage).
narrative_ontology:cs_interpretation_layer_present('8316fa50-11ef-43b0-82d8-74614607a2df').
narrative_ontology:cs_reading_relation('8316fa50-11ef-43b0-82d8-74614607a2df', constitutional_conventions__collective_responsibility, influences).
narrative_ontology:cs_reading_relation('8316fa50-11ef-43b0-82d8-74614607a2df', constitutional_conventions__ministerial_responsibility, influences).
narrative_ontology:cs_reading_relation('8316fa50-11ef-43b0-82d8-74614607a2df', constitutional_conventions__salisbury_convention, coexists_with).
narrative_ontology:cs_axiom('8316fa50-11ef-43b0-82d8-74614607a2df', foundational, crown_legislative_will_subordinate_to_parliamentary_finality).
narrative_ontology:cs_axiom_status(crown_legislative_will_subordinate_to_parliamentary_finality, holdable).
narrative_ontology:cs_axiom_grounding('8316fa50-11ef-43b0-82d8-74614607a2df', crown_legislative_will_subordinate_to_parliamentary_finality, conventional).
narrative_ontology:cs_axiom('8316fa50-11ef-43b0-82d8-74614607a2df', foundational, assent_fiction_must_be_maintained).
narrative_ontology:cs_axiom_status(assent_fiction_must_be_maintained, holdable).
narrative_ontology:cs_axiom_grounding('8316fa50-11ef-43b0-82d8-74614607a2df', assent_fiction_must_be_maintained, instrumental).
narrative_ontology:cs_reference_frame('8316fa50-11ef-43b0-82d8-74614607a2df', sovereign_crown_legislative_authority).
narrative_ontology:cs_drift_state('8316fa50-11ef-43b0-82d8-74614607a2df', contemporary_democratic_age, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8316fa50-11ef-43b0-82d8-74614607a2df', '').
narrative_ontology:cs_kernel_id(constitutional_conventions__royal_assent_convention, constitutional_conventions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_conventions__royal_assent_convention, parliamentary_finality).
narrative_ontology:constraint_beneficiary(constitutional_conventions__royal_assent_convention, crown_institutional_survival).
narrative_ontology:constraint_victim(constitutional_conventions__royal_assent_convention, sovereign_legislative_will).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENT (ROPE) — The convention solves a genuine coordination problem: Parliament needs assurance that the Crown will not reverse legislation. The rule that the monarch assents to every duly passed bill enables parliamentary finality. This is pure coordination — the Crown's restraint secures Parliament's ability to govern. No extraction perceived because both parties benefit from the stable rule.
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CROWN (PITON) — The royal assent is pure theater: the monarch's legislative will has been structurally evacuated. The power to withhold assent exists in law (no act terminates it) but is functionally dead — any attempt to use it would trigger constitutional crisis and force abdication. The Crown maintains the ritual of assent because the alternative (explicit admission that the Crown cannot refuse) would expose the constraint's artificiality. The performance persists through institutional inertia: the ceremony of royal assent is performed at the State Opening, in parliamentary procedure, in formal documents. But the underlying power is inert.
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — From a transcendent vantage, the convention appears as an immutable law of constitutional nature: democracies inherently cannot allow unelected monarchs to veto legislation. The rule that the Crown must assent seems like a logical necessity, not a contingent institutional choice. However, the structural data reveals this as naturalization. The power is not impossible — it is suppressed. The convention is not a physical law — it is enforced by threat of abdication. The false summit detector will flag this: beneficiaries exist (parliamentary finality, crown institutional survival), theater is extreme (0.95), and suppression is high (0.72).
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: REIGNING MONARCH (SNARE) — Any individual occupying the Crown finds their legislative will structurally suppressed. The monarch is simultaneously sovereign in law and politically powerless in practice. The exit option is abdication (extreme cost). The extraction mechanism is the rule itself: the convention functions as a total suppression of the Crown's personal legislative agency. The reigning monarch is trapped by the unwritten rule that must never be articulated as a rule — to speak the suppression would break the fiction that assent is voluntary.
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: THE MONARCHY AS INSTITUTION (TANGLED ROPE) — The Crown as an institution benefits from the convention: it provides political neutrality and survival justification. The monarchy survives in a democratic age precisely because it assents to legislation and does not govern. But this benefit comes at the cost of institutional constraint: the Crown's political agency is permanently foreclosed. The institution experiences both coordination (parliamentary finality enables stable governance) and extraction (the Crown's own legislative capacity is nullified). Neither pure — a hybrid where institutional survival depends on accepting permanent subordination.
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM MOVEMENTS (SCAFFOLD) — Movements toward formal codification of the constitution (e.g., written constitutional reform, explicit abolition of the reserve powers) see the convention as a temporary, improvised solution pending permanent constitutional settlement. The convention's lack of formal status is treated as a design flaw to be corrected. This perspective views the constraint as transitional: once a written constitution is adopted, the royal veto will be formally and irrevocably abolished, and the theater of assent will be replaced with an explicit power-division rule. The sunset is the endpoint of constitutional reform. Low extractiveness from this perspective because reformers have agency and see a clear exit path.
constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_conventions__royal_assent_convention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_conventions__royal_assent_convention, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_conventions__royal_assent_convention, TR),
    TR >= 0.70.

:- end_tests(constitutional_conventions__royal_assent_convention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The convention suppresses the Crown's legislative will almost entirely, but the extraction is not severe in the Snare sense because (1) the Crown benefits institutionally from the subordination — it enables the monarchy's survival in a democratic system, and (2) the suppression is absolute and therefore crystallized rather than exploited. Once the power is completely dead, there is no ongoing extraction — the negotiation ended centuries ago. The initial value (0.15 at 1708) reflects that the constraint was new, contested, and the Crown still had some theoretical legislative agency. Over the interval, extractiveness fell as the power became a historical artifact. Theater ratio (0.95): Extremely high. The constraint is maintained almost entirely through performance: the ritual of royal assent is conducted at state openings, in parliamentary procedure, in formal proclamations. The underlying power is inert — no functional verification mechanism exists. The ceremony persists because to abandon it would require explicitly stating that the Crown cannot refuse, which would expose the fiction. This is the defining signature of a piton — institutional inertia maintaining a degraded function through continued performance. Suppression (0.72): High. The formal power to withhold assent exists in law but is suppressed by threat of constitutional crisis and abdication. This is not physical constraint but political enforcement: any reigning monarch who refused assent would face demands to abdicate within days. The suppression is not total (the law still permits assent-withholding) but absolute in practice (political reality makes it impossible). The beneficiary/victim structure reveals the asymmetry: Parliament gains certainty; the Crown loses agency.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. Parliament sees pure coordination (Rope): a rule that enables stable governance. The reigning monarch sees pure extraction (Snare): suppression of their legislative will. The monarchy-as-institution sees mixed extraction-coordination (Tangled Rope): institutional survival at the cost of permanent political neutrality. The Crown in historical form sees inert performance (Piton): a ceremony that persists through routine despite having no function. Constitutional reformers see a transitional problem (Scaffold): an improvised solution awaiting formal settlement. The analytical observer risks seeing an immutable principle (Mountain, false summit): as if democracies inherently cannot allow unelected monarchs legislative veto. The perspectival gap is not measurement disagreement — it is structural. Each agent genuinely occupies a different position in the constraint's extraction flow. No single classification captures all positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from the agent's structural relationship to the constraint. Parliament, as beneficiary with arbitrage options (can exit through constitutional reform but benefits from the current arrangement), derives low d → low/negative χ. The reigning monarch, as victim with only abdication as exit (trapped option), derives high d → high f(d) → high χ. The monarchy-as-institution, as a powerful institutional actor with constrained exit (the institution survives by accepting subordination), derives moderate d → moderate χ. The analytical observer at civilizational scope derives d from the observational position (0.72 canonical for analytical) before the false summit detector fires on the beneficiary presence and extreme theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification resolves the mandatrophy by showing that the constraint is NOT a natural law of constitutional necessity — it is an institutional arrangement that persists through performance. The false-summit detector will flag the mountain perspective (analytical/civilizational/universal view that sees the suppression as inevitable) because: (1) beneficiaries exist (parliamentary finality), (2) theater is extreme (0.95), (3) suppression is high (0.72), and (4) no natural-law signatures are present (accessibility_collapse and resistance are not declared, per the mountain gate). The piton classification is correct: the constraint is a performative maintenance of a degraded function, held in place by the rule that the rule must never be stated. The institutional actors know this — they maintain the fiction precisely because stating the truth would break the system. The mandatrophy resolves in the presheaf: all six types are legitimate readings of the same structural data from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_volitional_or_coercive,
    'Is the Crown''s adherence to the convention a voluntary choice (accepting constraint for institutional benefit) or coercive suppression (threat of abdication/constitutional crisis)?',
    'Historical counterfactual analysis: what would happen if a reigning monarch explicitly refused assent? Comparative analysis: do other democracies with unelected heads of state show similar patterns? Explicit statements from the Crown about the perceived ''choice'' to assent.',
    'If voluntary: piton classification weakens — the constraint becomes rope (coordination accepted by all parties). If coercive: snare classification strengthens — the Crown''s agency is suppressed against its will. The reading''s core claim depends on this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_volitional_or_coercive, empirical, 'Whether suppression of royal veto is voluntary institutional choice or coercive threat').

omega_variable(
    extractiveness_of_legislative_nullification,
    'Does the complete functional elimination of the Crown''s legislative veto constitute extraction from the Crown, or is it a fair coordination cost?',
    'Comparative study of constitutional settlements: how do other monarchies distribute legislative power? Historical analysis of Crown proposals or legislative interests that were foregone due to the convention. Interviews with constitutional historians about whether the Crown perceives itself as having ''lost'' power or ''gained'' stability.',
    'If nullification is pure extraction: victim is ''sovereign_legislative_will'' (abstract loss of power) and extractiveness should rise to 0.20–0.30. If it is fair coordination cost: victims list is empty or reformulated, and extractiveness falls to 0.05. The piton classification''s theater gate would hold regardless, but the underlying story changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_of_legislative_nullification, conceptual, 'Whether legislative veto nullification is extraction or coordination cost').

omega_variable(
    kernel_vs_convention_status,
    'Is this constraint a true constitutional convention (unwritten rule enforced by political sanction) or a kernel commitment (foundational principle of the system)? Does the distinction matter for classification?',
    'Analyze whether the convention is invoked as a governing principle in constitutional disputes (kernel behavior) or merely as a descriptive rule about how things are done (convention behavior). Test: would explicit codification of the rule change its force, or would it simply formalize what is already enforced?',
    'If kernel: the CS_structure is correct and the rule derives legitimacy from constitutional fundamentals. If mere convention: the constraint is more fragile than mountain classification suggests — it depends entirely on political sanction, not on structural necessity. Piton classification becomes more appropriate (theater maintaining a practice that could be abandoned).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_convention_status, conceptual, 'Whether the rule is a true constitutional kernel or merely a convention').

omega_variable(
    reading_sibling_foreclosure,
    'Does the royal assent convention foreclose, influence, or coexist with the sibling readings (collective responsibility, ministerial responsibility, salisbury convention)?',
    'Logical analysis of each sibling''s core premise against this reading''s core premise. Historical evidence of whether defending one reading requires denying another. Constitutional case law testing whether the readings are treated as mutually exclusive or complementary.',
    'Affects cs_structure.reading_relations declarations. If any sibling is foreclosed, the kernel itself is contested (one framework cannot hold both readings). If all coexist, the kernel is a presheaf where different parties instantiate different readings simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Logical relationships between this reading and sibling readings in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_conventions__royal_assent_convention, 1708, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(royalassent_theater_1708, constitutional_conventions__royal_assent_convention, theater_ratio, 0, 0.85).
narrative_ontology:measurement(royalassent_theater_1808, constitutional_conventions__royal_assent_convention, theater_ratio, 100, 0.92).
narrative_ontology:measurement(royalassent_theater_1908, constitutional_conventions__royal_assent_convention, theater_ratio, 200, 0.95).
narrative_ontology:measurement(royalassent_theater_2024, constitutional_conventions__royal_assent_convention, theater_ratio, 316, 0.95).

% Extraction over time
narrative_ontology:measurement(royalassent_extractiveness_1708, constitutional_conventions__royal_assent_convention, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(royalassent_extractiveness_1808, constitutional_conventions__royal_assent_convention, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(royalassent_extractiveness_1908, constitutional_conventions__royal_assent_convention, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(royalassent_extractiveness_2024, constitutional_conventions__royal_assent_convention, base_extractiveness, 316, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_conventions__royal_assent_convention, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_conventions__royal_assent_convention, constitutional_conventions__collective_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__royal_assent_convention, constitutional_conventions__ministerial_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__royal_assent_convention, constitutional_conventions__salisbury_convention).

% DUAL FORMULATION NOTE:
% All four readings (royal_assent_convention, collective_responsibility, ministerial_responsibility, salisbury_convention) are distinct constraints operating on the same kernel. They form a constraint family linked by network.affects_constraints. Each has its own ε and perspectival profile. The family's overall structure is a presheaf: different parties instantiate different readings simultaneously; no single reading is universally held. The constitutional system's stability depends on this distributed disagreement remaining unresolved — any attempt to formally codify one reading would force resolution and break the fiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_conventions__royal_assent_convention, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
