% ============================================================================
% CONSTRAINT STORY: civil_rights_era_amendments__twenty_fifth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_rights_era_amendments__twenty_fifth_amendment, []).

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
 *   constraint_id: civil_rights_era_amendments__twenty_fifth_amendment
 *   human_readable: Twenty-Fifth Amendment: Presidential Disability and Succession Machinery
 *   domain: political/constitutional_law
 *
 * SUMMARY:
 *   The Twenty-Fifth Amendment (ratified 1967) codifies the machinery for
 *   presidential succession when the executive is incapacitated. The
 *   Constitution's framers left this gap unspecified — succession on death
 *   (addressed by Article II) but not on disability. The Amendment provides
 *   four sections: Section 1 transfers power on presidential
 *   death/resignation; Sections 3 and 4 establish voluntary declaration and
 *   involuntary removal machinery for disability; Section 2 addresses
 *   vice-presidential vacancy. The constraint exhibits all six classification
 *   types from different perspectives. The core extraction mechanism it
 *   suppresses is power-seizure through ambiguity — cabinet members, vice
 *   presidents, or Congress could exploit succession ambiguity to seize or
 *   hold executive power. The Amendment's beneficiary is constitutional
 *   continuity and executive stability; its victim set is the prior ambiguity
 *   (which made unauthorized removal possible) and the vice president (whose
 *   authority is regularized rather than autonomous). The theater ratio has
 *   risen over 50+ years as the formal machinery has become increasingly
 *   performative: disability is routinely ambiguous, the Amendment's text
 *   addresses only clear cases, and political incentives discourage using
 *   Sections 3-4 machinery (fear of precedent, legitimacy costs). This
 *   reading is one of four siblings in the civil_rights_era_amendments kernel
 *   — the Twenty-Fourth (poll tax), Twenty-Sixth (voting age), and
 *   Twenty-Third (DC votes) amendments address different beneficiary/victim
 *   structures within the same constitutional era.
 *
 * KEY AGENTS:
 *   - Constitutional Continuity: Primary beneficiary (institutional/arbitrage) — the executive office benefits from regularized succession machinery
 *   - Ambiguity Vulnerability: Primary victim (powerless/trapped) — the pre-Amendment constitutional gap exposed governance to seizure through uncertainty
 *   - Vice President: Secondary actor (organized/constrained) — Sections 3-4 grant unusual removal power but bind the VP to cabinet/congressional mechanics; authority is constrained rather than autonomous
 *   - Congress: Secondary actor (institutional/constrained) — gains unusual power to adjudicate presidential fitness but inherits institutional friction with executive
 *   - Cabinet: Secondary actor (organized/constrained) — gains removal initiation power but bound to congressional ratification; extraction risk if weaponized
 *   - Incapacitated Presidents: Secondary victim/beneficiary (moderate/constrained) — benefit from formal disability accommodation but face removal risk if Congress votes to sustain
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent constitutional choice as inherent governance requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_rights_era_amendments__twenty_fifth_amendment, 0.28).
domain_priors:suppression_score(civil_rights_era_amendments__twenty_fifth_amendment, 0.35).
domain_priors:theater_ratio(civil_rights_era_amendments__twenty_fifth_amendment, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fifth_amendment, extractiveness, 0.28).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fifth_amendment, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fifth_amendment, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_rights_era_amendments__twenty_fifth_amendment, tangled_rope).
narrative_ontology:human_readable(civil_rights_era_amendments__twenty_fifth_amendment, "Twenty-Fifth Amendment: Presidential Disability and Succession Machinery").
narrative_ontology:topic_domain(civil_rights_era_amendments__twenty_fifth_amendment, "political/constitutional_law").

domain_priors:requires_active_enforcement(civil_rights_era_amendments__twenty_fifth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(civil_rights_era_amendments__twenty_fifth_amendment, '3769bf1a-3e02-41e5-a3e4-9d46d4010900').
narrative_ontology:cs_kernel_codification('3769bf1a-3e02-41e5-a3e4-9d46d4010900', formalized).
narrative_ontology:cs_authority_grounding('3769bf1a-3e02-41e5-a3e4-9d46d4010900', lineage).
narrative_ontology:cs_interpretation_layer_present('3769bf1a-3e02-41e5-a3e4-9d46d4010900').
narrative_ontology:cs_reading_relation('3769bf1a-3e02-41e5-a3e4-9d46d4010900', civil_rights_era_amendments__twenty_fourth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('3769bf1a-3e02-41e5-a3e4-9d46d4010900', civil_rights_era_amendments__twenty_sixth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('3769bf1a-3e02-41e5-a3e4-9d46d4010900', civil_rights_era_amendments__twenty_third_amendment, coexists_with).
narrative_ontology:cs_axiom('3769bf1a-3e02-41e5-a3e4-9d46d4010900', foundational, presidential_disability_machinery_necessary).
narrative_ontology:cs_axiom_status(presidential_disability_machinery_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3769bf1a-3e02-41e5-a3e4-9d46d4010900', presidential_disability_machinery_necessary, instrumental).
narrative_ontology:cs_axiom('3769bf1a-3e02-41e5-a3e4-9d46d4010900', foundational, succession_ambiguity_suppression).
narrative_ontology:cs_axiom_status(succession_ambiguity_suppression, holdable).
narrative_ontology:cs_axiom_grounding('3769bf1a-3e02-41e5-a3e4-9d46d4010900', succession_ambiguity_suppression, conventional).
narrative_ontology:cs_reference_frame('3769bf1a-3e02-41e5-a3e4-9d46d4010900', constitutional_succession_clarity).
narrative_ontology:cs_drift_state('3769bf1a-3e02-41e5-a3e4-9d46d4010900', contemporary_gray_zone_incapacity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3769bf1a-3e02-41e5-a3e4-9d46d4010900', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(civil_rights_era_amendments__twenty_fifth_amendment, civil_rights_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_fifth_amendment, constitutional_continuity).
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_fifth_amendment, executive_stability).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_fifth_amendment, ambiguity_vulnerability).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_fifth_amendment, vice_presidential_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMBIGUITY VULNERABILITY (SNARE) — The constitutional gap that existed before the Twenty-Fifth Amendment — the lack of machinery for presidential disability — exposed the body politic to seizure through ambiguity. Any incapacitated president created a power vacuum. Those trapped in this vulnerability (citizens, Congress, the incapacitated executive themselves) bore the full cost of uncertainty. The Amendment suppresses this extractive gap, but from the perspective of actors who benefit from ambiguity (ambitious cabinet members, vice presidents seeking power without constitutional machinery), the Amendment forecloses their extraction mechanism.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESS AND VICE PRESIDENT (TANGLED ROPE) — The Twenty-Fifth Amendment imposes a coordination function (resolving presidential disability without constitutional chaos) while extracting authority from both the vice president and Congress. Sections 3 and 4 allow the vice president plus cabinet to remove a president provisionally; Congress must ratify with a supermajority. Both branches experience the Amendment as mixed: genuine coordination of succession machinery (benefit), but with embedded asymmetric authority (the vice president gains unusual removal power; Congress gains unusual presidential judgment powers). Suppression exists because neither can easily exit this framework once established.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL CONTINUITY (ROPE) — From the institutional perspective of constitutional order and executive stability, the Twenty-Fifth Amendment is pure coordination: it solves a genuine collective action problem (presidential succession ambiguity) with minimal coercive overhead. The Amendment enables the executive to remain staffed and functional across disability events. No institution is extracted from — the machinery serves legitimate succession needs. The beneficiary is the continuity and stability of the executive office itself.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCAPACITATED EXECUTIVES (SCAFFOLD) — The Amendment provides temporary machinery for handling disability without permanent removal. Section 3 allows a president to voluntarily declare their own disability and recover when able. This creates a low-extraction framework for executives with temporary incapacity: the authority to manage disability is temporary and returnable. However, Sections 3 and 4 together create risk — if Congress votes to sustain a removal, the president loses office permanently. The framework is scaffolding: it is functional and time-bounded for legitimate disability, but contains extractive risk if weaponized against a recovering executive.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AMBIGUITY AND GRAY ZONES (PITON) — The Twenty-Fifth Amendment's text provides machinery for *clear* disability (Section 3: presidential declaration; Section 4: cabinet and congressional action). But most presidential incapacity is ambiguous — cognitive decline, mental health crises, substance abuse, extreme fatigue. The Amendment's machinery is performative around ambiguous incapacity: it cannot and does not resolve what counts as disabling. The ritual of assessment (medical exams, congressional hearings) is substantial, but the underlying ambiguity persists. The Amendment's theater ratio is high because it channels ambiguity into formal process without eliminating the ambiguity itself. Over time, the machinery may degrade into theater as political actors avoid using it (fear of precedent, legitimacy questions). The constraint persists through inertia — the Amendment exists as law but functionally degrades when its machinery threatens political power.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational analytical perspective, some machinery for executive succession is constitutive of viable governance itself. No state can function with a permanently incapacitated executive and no succession machinery — the gap would be a structural impossibility, not a constitutional choice. The Twenty-Fifth Amendment is sometimes framed as discovering a natural requirement of viable constitutions, not creating a new extraction mechanism. However, the structural data reveals this as a false summit: the Amendment is a constructed choice among alternative succession framings (some with more vice-presidential power, some with more congressional power, some with more executive discretion). The machine itself is contingent.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_rights_era_amendments__twenty_fifth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fifth_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(civil_rights_era_amendments__twenty_fifth_amendment, TR),
    TR >= 0.70.

:- end_tests(civil_rights_era_amendments__twenty_fifth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28, declining from 0.42): The Amendment suppresses extraction through ambiguity — without it, succession gaps would be exploitable. The initial extractiveness (0.42) reflects the vulnerability to power-seizure the Amendment closes. Over 50+ years, extractiveness declines as the machinery becomes normalized and the ambiguity vulnerability is culturally foreclosed — it becomes unthinkable to seize power through succession gaps. Current value (0.28) reflects that the Amendment's coordination function dominates its extractive potential. However, residual extraction exists: the machinery enables Congress and the cabinet to remove presidents on disability grounds, creating risk of weaponization. Suppression (0.35): Moderate. The Amendment constrains (but does not eliminate) alternatives to formal succession. A president can declare their own disability (Section 3), reducing suppression. But Sections 3-4 together suppress the option of autonomous executive self-management — the president cannot permanently resist removal if Congress and cabinet align. Theater ratio (0.52, rising from 0.35): The machinery is increasingly performative. Clear disabilities (death, resignation, explicit medical incapacity) are handled routinely outside formal Sections 3-4 process. Ambiguous disabilities (cognitive decline, mental health, fatigue) fall into a gray zone the Amendment does not address. The formal process (medical exams, congressional hearings) is theater around an irreducibly ambiguous problem. Theater rises as political actors learn to avoid the machinery (fear of precedent), treating it as constitutional insurance rather than functional mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The Twenty-Fifth Amendment generates a full perspectival range. The institutional perspective sees pure coordination (Rope) — regularized succession machinery solves a genuine collective action problem. The powerless victim perspective (ambiguity vulnerability) sees Snare — the prior constitutional gap was extractive, and while the Amendment closes it, those trapped in the vulnerability bore full cost. The organized actors (VP, Congress) see Tangled Rope — genuine coordination of succession alongside embedded authority asymmetries. Incapacitated executives see Scaffold — temporary formal machinery with sunset logic (recovery is allowed) but removal risk. The performative assessment machinery appears as Piton — the ritual of disability determination persists but lacks functional force for ambiguous cases. The civilizational analytical observer risks seeing Mountain — a natural constitutional requirement — but the structural data (contingent choice among alternatives, identified beneficiaries, suppressible ambiguity) flags this as false summit. The perspectival gap reveals that the Amendment's type depends entirely on which structural position you measure from.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position. Constitutional Continuity (institutional/arbitrage) experiences low d ≈ 0.10 — it is the beneficiary of orderly succession and faces no extraction. Ambiguity Vulnerability (powerless/trapped) experiences high d ≈ 0.95 — it is the victim and cannot exit the constitutional gap. Vice President (organized/constrained) experiences moderate d ≈ 0.50 — gains unusual authority (beneficiary features) but is bound to cabinet/Congress (victim features). Incapacitated Presidents (moderate/constrained) experience mixed d ≈ 0.60 — benefit from formal disability accommodation but face removal risk. Congress and Cabinet (institutional/constrained) experience d ≈ 0.45 — gain removal authority but inherit friction with executive. Analytical Observer (analytical/analytical) experiences canonical d ≈ 0.72. The derivation produces a perspectival gap: Rope perspective for institutional beneficiary (continuity), Snare for powerless victim (ambiguity), Tangled Rope for organized and moderate actors (mixed authority and extraction), Piton for the performative assessment machinery, Mountain for the natural law view (which the structural data flags as false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   The Twenty-Fifth Amendment exhibits the mandatrophy pattern at the kernel level. Within the single kernel (civil_rights_era_amendments), the 25th, 24th, 26th, and 23rd readings are all live, legitimate constitutional readings, but they address structurally distinct problems: disability machinery, economic franchise barriers, age-based voting exclusion, and capital city representation. The mandatrophy is NOT about which amendment is 'correct' — they are all correct readings addressing different domains. The mandatrophy for the 25th reading specifically is resolved by recognizing that all six classification types emerge from legitimate perspectival differences about the same structural machinery. The false summit (mountain perspective) naturalizes what is actually a contingent institutional choice. The rope perspective (institutional continuity) captures the genuine coordination function. The snare perspective (ambiguity vulnerability) captures the prior extractive gap. The tangled rope perspective (organized actors) captures the mixed authority structure. The piton perspective (assessment theater) captures the machinery's performative dimension. No single type is 'the' answer — the presheaf of perspectives IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_vs_pathology,
    'Does the Twenty-Fifth Amendment suppress extractive power-seizure through ambiguity, or does it relocate extraction into the formal machinery of disability assessment?',
    'Comparative institutional analysis: Track cases where Section 3 (voluntary declaration) was invoked vs. Section 4 (cabinet/congressional removal). Assess whether the formal machinery prevented extraction or merely formalized it. Compare to counterfactual: what extraction would have occurred under pre-Amendment ambiguity governance?',
    'If Amendment suppresses extraction: ε ≈ 0.15 (Rope). If Amendment relocates extraction into assessment machinery: ε ≈ 0.45 (Tangled Rope). If Amendment both suppress and relocates, current classification (0.28) is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_vs_pathology, empirical, 'Whether the Amendment suppresses or relocates extraction').

omega_variable(
    disability_determination_authority,
    'Who has final authority to determine presidential disability — the president themselves, the cabinet, Congress, courts, or some hybrid? Does the Amendment''s answer represent a settled constitutional commitment, or a contestable reading?',
    'Constitutional law scholarship and case analysis. Assess whether courts would defer to Congress, cabinet, or presidential self-declaration in a disputed case. Examine whether the Amendment forecloses alternative succession readings or merely expresses a preference.',
    'If authority is clearly established: Amendment is Rope (genuine coordination). If authority is ambiguous: Amendment is Piton (theater masking residual ambiguity). If alternative readings remain live: Amendment Coexists with other succession readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disability_determination_authority, conceptual, 'Authority structure for disability determination').

omega_variable(
    false_summit_constitution,
    'Is the Twenty-Fifth Amendment discovering a natural constitutional requirement (all viable constitutions must have disability machinery), or is it a contingent institutional choice among alternatives?',
    'Comparative constitutionalism: examine how other democracies handle presidential disability. Assess whether the variety of alternatives suggests contingency or whether a core machinery requirement appears across all viable constitutions.',
    'If natural law discovery: mountain classification is defensible. If contingent choice: false summit triggered, reclassifies toward Rope or Tangled Rope depending on beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_constitution, conceptual, 'Whether Amendment discovers natural constitutional requirement or expresses contingent choice').

omega_variable(
    kernel_reading_contest,
    'The Twenty-Fifth Amendment is one reading of the civil_rights_era_amendments kernel. What structural feature distinguishes this reading from the Twenty-Fourth (poll tax abolition), Twenty-Sixth (voting age), and Twenty-Third (DC electoral votes) readings?',
    'Comparative kernel analysis: Each amendment is a commitment to different beneficiary/victim structures. 25th = disability machinery beneficiary, ambiguity vulnerability victim. 24th = electoral access beneficiary, economic franchise barrier victim. 26th = voting franchise beneficiary, age-based disenfranchisement victim. 23rd = DC political voice beneficiary, capital population exclusion victim. The structural deltas emerge from the specific domain each reading addresses.',
    'If readings foreclose each other: only one can be held in a coherent framework (rare). If readings coexist: all remain live positions across different parties/framings (likely). If readings influence each other: one reading''s authority or resource conditions shape the others (expected).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between 25th and sibling constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_rights_era_amendments__twenty_fifth_amendment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tfamend_tr_t0, civil_rights_era_amendments__twenty_fifth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tfamend_tr_t15, civil_rights_era_amendments__twenty_fifth_amendment, theater_ratio, 15, 0.45).
narrative_ontology:measurement(tfamend_tr_t30, civil_rights_era_amendments__twenty_fifth_amendment, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(tfamend_be_t0, civil_rights_era_amendments__twenty_fifth_amendment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tfamend_be_t15, civil_rights_era_amendments__twenty_fifth_amendment, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(tfamend_be_t30, civil_rights_era_amendments__twenty_fifth_amendment, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_rights_era_amendments__twenty_fifth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fifth_amendment, presidential_succession_continuity).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fifth_amendment, cabinet_authority_scope).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fifth_amendment, congressional_removal_power).

% DUAL FORMULATION NOTE:
% The Twenty-Fifth Amendment is a kernel reading, not a constraint family member. It is linked to siblings (24th, 26th, 23rd amendments) via kernel structure, not via constraint network decomposition. The network.affects_constraints array points to downstream structural constraints shaped by the Amendment's machinery: how the VP's authority is constrained, what congressional removal power entails, and how executive succession continuity is maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
