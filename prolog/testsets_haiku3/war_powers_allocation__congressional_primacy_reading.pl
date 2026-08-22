% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested constitutional
 *   kernel: the allocation of war power between Congress and the executive.
 *   The congressional primacy reading asserts that the Constitution vests the
 *   decision to commit the nation to sustained military force in Congress
 *   ('Congress shall have Power ... To declare War'), and that executive
 *   unilateral commitment beyond immediate self-defense constitutes
 *   extraction of war-power authority that the Constitution reserves to the
 *   legislature. The reading is in active contest with two sibling readings:
 *   the inherent executive reading (commander-in-chief power includes
 *   inherent authority to deploy force without prior authorization) and the
 *   functional accommodation reading (authorization requirement varies by
 *   operational context—imminent threats permit unilateral action; prolonged
 *   campaigns require authorization). This story instantiates the
 *   congressional primacy reading alone, treating it as a clean, ε-invariant
 *   constraint. The other readings are separate constraint stories, linked
 *   through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Legislative branch: institutional custodian and beneficiary of the war power authority, but simultaneously a victim when bypassed; enforces through constitutional invocation, appropriations, and War Powers Resolution procedures
 *   - Executive branch: structurally constrained by the authorization requirement; must seek prior approval for sustained force beyond immediate defense or accept constitutional violation and political consequences
 *   - Congress enforcement agents (individual members): bear political cost of visibility and accountability in authorization votes; enforce through procedural and budgetary mechanisms
 *   - Courts: analytical observer; weak enforcement capacity due to political question doctrine and structural reluctance to enjoin military operations mid-deployment
 *   - Excluded parties: foreign populations affected by military action, military personnel subject to deployment (identity-locked), inherent authority claimants (whose position contradicts this reading's axiom)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.79).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'd1909151-2932-4500-af94-24b5957812d5').
narrative_ontology:cs_kernel_codification('d1909151-2932-4500-af94-24b5957812d5', fixed_text).
narrative_ontology:cs_authority_grounding('d1909151-2932-4500-af94-24b5957812d5', lineage).
narrative_ontology:cs_interpretation_layer_present('d1909151-2932-4500-af94-24b5957812d5').
narrative_ontology:cs_reading_relation('d1909151-2932-4500-af94-24b5957812d5', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('d1909151-2932-4500-af94-24b5957812d5', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('d1909151-2932-4500-af94-24b5957812d5', foundational, congressional_primacy_constitutional_mandate).
narrative_ontology:cs_axiom_status(congressional_primacy_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d1909151-2932-4500-af94-24b5957812d5', congressional_primacy_constitutional_mandate, deontological).
narrative_ontology:cs_axiom('d1909151-2932-4500-af94-24b5957812d5', foundational, sustained_force_requires_explicit_authorization).
narrative_ontology:cs_axiom_status(sustained_force_requires_explicit_authorization, holdable).
narrative_ontology:cs_axiom_grounding('d1909151-2932-4500-af94-24b5957812d5', sustained_force_requires_explicit_authorization, empirically_contingent).
narrative_ontology:cs_reference_frame('d1909151-2932-4500-af94-24b5957812d5', framers_separation_of_powers_design).
narrative_ontology:cs_drift_state('d1909151-2932-4500-af94-24b5957812d5', contemporary_practice_post_2001, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d1909151-2932-4500-af94-24b5957812d5', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_text_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, Congress is the custodian of the war power authority granted by the Constitution. Congress sets the rule (authorization required for sustained military force) and administers its enforcement through appropriations, War Powers Resolution procedures, and constitutional rhetoric. Congress simultaneously bears the cost: individual members must vote (political exposure and accountability), and Congress as a whole must accept violations when the executive acts unilaterally and forces a retroactive choice (authorize, defund, or accept the breach). The institutional agenda-setter is also the institutional victim when the rule is violated.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, legislative_branch, payer).

% Structurally constrained by the authorization requirement. The executive must seek prior congressional authorization before committing the nation to sustained military force (beyond immediate self-defense). If the executive acts unilaterally, it faces potential political and legal consequences: Congressional defunding, impeachment, adverse court rulings (rare but possible), damage to legitimacy and institutional reputation. The executive can act first and seek retroactive authorization, but this violates the constitutional rule and requires managing Congressional response. The executive's claimed inherent authority (commander-in-chief power) is suppressed by this reading.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Individual Congressional members and committees who enforce the authorization requirement. They invoke constitutional rhetoric, introduce legislation requiring authorization, use appropriations restrictions, invoke the War Powers Resolution, hold hearings, and in extreme cases initiate impeachment. They bear the political cost of enforcement: voting to authorize (or oppose) is public, creates accountability, and exposes them to blame if the authorized (or unauthorized) action has bad consequences. Their enforcement power is real but incomplete: the executive can act unilaterally and force Congress to choose retroactively.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_enforcement_agents, agenda_setter,
    institutional, biographical, constrained, national).

% Possess analytical authority to interpret the Constitution but have weak enforcement capacity in war powers disputes. Most war powers cases are dismissed as non-justiciable political questions, on the theory that Congress and the executive must resolve the dispute themselves. Courts have occasionally opined that authorization is constitutionally required but have rarely enjoined executive action mid-deployment. Their role is primarily interpretive, not enforcement.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, courts, observer,
    institutional, generational, constrained, national).

% Bear the material consequences of military action—casualties, displacement, economic disruption, destabilization—but have no seat in the authorization process. They are excluded from the domestic constitutional conversation; the question of whether the action was authorized by Congress is a domestic institutional matter to foreign populations, not a protection or justification. Authorization status does not materially reduce their exposure to harm.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, affected_foreign_populations, excluded,
    powerless, biographical, trapped, global).

% Execute military operations ordered by the executive, whether or not Congress has authorized them. They bear the risk of deployment (casualty, injury, moral liability) regardless of authorization status. Their professional identity is fused with obedience to lawful orders (as the military interprets them); they are not positioned to adjudicate whether the order exceeds constitutional authority. Escape would require accepting discharge or legal consequences. They pay through risk exposure; they are excluded from the authorization decision.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, american_military_personnel, excluded,
    powerless, biographical, identity_locked, national).

% Constitutional scholars, executive branch lawyers, and officials who hold that presidential commander-in-chief power includes inherent authority to deploy force without prior congressional authorization. This reading (congressional primacy) explicitly suppresses their claim by asserting that the Constitution vests war power in Congress, and that invoking inherent authority is a suppression mechanism—a rhetorical counter to the authorization requirement. They are excluded because their core position contradicts this reading's foundational axiom. Their alternative reading is a sibling story, not part of this constraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, inherent_authority_claimants, excluded,
    institutional, biographical, constrained, national).

% Constitutional scholars and officials who argue war powers allocation should vary by operational context: imminent threats permit unilateral executive action; prolonged campaigns require congressional authorization; intermediate cases depend on judgment and negotiation. This reading (congressional primacy) rejects that framing as a compromise that erodes the authorization requirement in practice by expanding the 'imminent threat' exception. They are excluded because their contextual approach contradicts this reading's assertion of a binding, non-contextual requirement. Their alternative reading is a sibling story.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, functional_accommodation_proponents, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the decision to commit the nation to sustained military force across two branches of government: Congress authorizes; the executive implements and commands. The constraint solves the coordination problem of who decides on war. The reading asserts Congress must decide first, in advance of deployment. This embodies a structural allocation designed to require deliberation and political accountability before military commitment.
% TRANSFER_FUNCTION: Transfers war-power authority from potential executive unilateralism to a joint executive-legislative process. The executive loses the sole discretion to deploy force and must seek authorization. Congress gains the authority to say no (or yes, or condition authorization). Congress bears the cost of voting (visibility, accountability, political consequences). The executive bears the cost of seeking authorization before acting, or the cost of acting unilaterally and facing retroactive Congressional response (defunding, impeachment, loss of legitimacy).
% ABSENT_VOICES: Foreign populations subject to military action have no constitutional voice in the authorization process. Military personnel subject to deployment are structurally excluded (professional subordination, no right to question orders). Domestic populations bear the tax cost and risk of reciprocal attack but have only indirect representation through their Congressional representatives. Scholars and officials who hold alternative constitutional readings (inherent executive authority, functional accommodation) are excluded from this constraint's framework—they are the subjects of the suppression mechanism, not voices within it.
% DISAPPEARANCE_RATIONALE: If the authorization requirement disappeared and the executive had unilateral power to deploy force, the constitutional separation of powers would collapse in this domain. Congress would lose its primary institutional check on executive military power. The United States' strategic posture, international relations, domestic politics, and constitutional structure would reorganize around executive discretion. Unilateral military commitment would become constitutionally unconstrained.
% FOUNDING_PROBLEM: The Framers of the Constitution were alarmed by European monarchical practice: kings commit nations to war for dynastic interest, leading to chronic conflict and waste. They designed the Constitution to prevent this by dividing the war power: Congress holds the power to declare war; the president commands the military. Congress decides whether to commit; the president decides how to fight. The founding problem was preventing concentrated war power in a single executive and ensuring that military commitment required deliberation and political accountability.
% FOUNDING_PROBLEM_CORROBORATION: Originalist historians and scholars (e.g., David Barron, analysis of Founding-era documents, Federalist Papers commentary on the war power) attest that the Framers intended to constrain executive unilateralism in military commitment. Historical scholarship establishes the Framers' concern about monarchy and their design to prevent it. However, corroboration is contested by the institutional party (the executive branch) that benefits from the alternative reading. Executive branch legal opinions and scholars claiming inherent authority cite different historical sources and reach different conclusions. Congressional testimony and War Powers Resolution legislative history provide corroboration outside the benefiting parties, but the executive's counter-reading complicates simple historical claims.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 → 0.68) because historical practice shows increasing instances of substantial military deployment without explicit prior authorization—drone strikes, cyber operations, military advisors, extended air campaigns. The constraint as written (authorization required) is increasingly violated, and the executive's extraction of unilateral authority is increasingly successful. Suppression also rises (0.55 → 0.79) because maintaining the authorization requirement requires active enforcement through constitutional rhetoric, appropriations restrictions, and War Powers Resolution invocation; as unilateral action normalizes, suppression must intensify to maintain the rule. Theater rises moderately (0.25 → 0.42) because much authorization-debate discourse serves theatrical functions (Congressional cover for executive decisions already made, public theater of dissent that does not block action). The interpretation: the constraint describes a real coordination function (who decides on war?) and a real asymmetry (Congress must vote; the executive can act unilaterally and force retroactive authorization or acceptance). Measurements are authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative branch's perspective, the constraint is a boundary-maintenance rule: Congress sets the agenda (authorization requirement), and the executive violates it by unilateral action. Congress is simultaneously beneficiary (controls the power) and victim (pays the enforcement cost and is violated). From the executive's perspective, the constraint is an obstruction to rapid response; the executive reads the authorization requirement as procedurally burdensome, not as core constitutional allocation. From the courts' perspective, the constraint is a political question—the branches are expected to resolve it, not the judiciary. From foreign populations and military personnel's perspective, the constraint is entirely abstracted; authorization status does not materially change the consequences of deployment. The engine computes these divergent seats from the structural data: legislative frustration and dual positioning should produce a different computed type than executive chafing, because the structural relationship to the constraint differs (agenda-setter-and-victim vs. constrained-actor).
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch holds paradoxical position: it is the beneficiary of the war power authority (the constraint vests the power in Congress) and the victim of its violation (the constraint is regularly breached when the executive acts unilaterally). Directionality derivation: Congress benefits from the rule's stable operation (d toward beneficiary end); Congress pays when the rule is violated (d toward target end). The net d should reflect this duality—moderate, near-symmetric, because the same institutional actor sets and enforces the rule while being the one harmed when it breaks. The executive is structurally a target (constrained, must seek authorization or face consequences; d toward target end). Courts are analytical observers (d near analytical baseline). Excluded parties have no exit options and no structural choice in the arrangement (powerless, trapped). The authorization requirement's effectiveness depends on Congress's willingness to enforce it; as extraction (unilateral action) succeeds in practice, Congress's enforcement burden rises and the rule's credibility erodes.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is a live question here: has the founding problem (preventing executive unilateralism in military commitment) outlived its functional relevance? Historical practice suggests the authorization requirement is increasingly honored in the breach, not the practice. Unilateral actions (drone strikes, cyber operations, military advisors in active zones) continue despite the constraint. However, mandatrophy is contested because: (1) the founding problem remains structurally live—the Framers' concern about executive unilateralism in war persists as a theoretical issue; (2) some authorization activity still occurs (congressional AUMF votes, though increasingly general and blanket); (3) the constraint's suppression must intensify precisely because the mandate is challenged. This is not a Piton (performance without function) but a Tangled Rope under stress: real coordination function (who decides?) and real extraction (executive unilateral authority-taking) both present, but the enforcement machinery is eroding. The measurement trajectory (rising extractiveness, rising suppression theater) captures this stress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_boundary_ambiguity,
    'What constitutes ''immediate defense'' versus ''sustained military force requiring authorization''? Where is the temporal and operational boundary?',
    'Judicial clarification (rare); legislative definition via statute (e.g., War Powers Resolution refinement); historical case accumulation establishing de facto boundary; international law precedent on self-defense duration limits.',
    'If the boundary is narrow (hours to days), more executive actions require authorization and extractiveness is contained. If the boundary is wide (weeks, months of ''immediate response''), the executive has substantially more unilateral discretion and extractiveness rises. This reading asserts a narrow boundary; the functional accommodation reading treats the boundary as contextual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_boundary_ambiguity, empirical, 'Definitional ambiguity in ''immediate defense'' vs. ''sustained force'' allowing executive exploitation of the boundary zone.').

omega_variable(
    congressional_enforcement_capacity,
    'Can Congress enforce the authorization requirement in real time, or is enforcement necessarily retroactive and conditional (retroactive authorization, defunding existing operations, impeachment)?',
    'Observational study of war powers disputes: do preventive injunctions ever issue? Does defunding occur before operations complete? Do retroactive authorizations regularize or block unilateral action?',
    'If enforcement is only retroactive, the constraint is weakened: the executive acts, Congress votes afterward (often rubber-stamping), and the authorization serves a theater function rather than a real gate. If enforcement is preventive, the constraint is stronger. This reading assumes preventive enforcement is possible; historical practice suggests enforcement is mostly retroactive, which undermines the reading''s efficacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_enforcement_capacity, empirical, 'Temporal mismatch between authorization requirement and enforcement capacity.').

omega_variable(
    competing_constitutional_reading,
    'Is this reading the reading of the Constitution''s text and original intent, or is it one plausible reading among several equally defensible interpretations?',
    'Originalist historical scholarship; constitutional convention notes and Federalist Papers analysis; contemporary Supreme Court precedent (noting that SCOTUS has avoided definitive war powers holdings); institutional practice over time.',
    'If this reading is THE reading (singular, texturally and historically supported), mandatrophy is false and the constraint is violated in practice. If this reading is ONE reading among equally defensible alternatives, the constraint is contested and the engine should classify it as such. The foundational axiom of this reading depends on the claim that congressional primacy IS the constitutional requirement, not one option among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_constitutional_reading, conceptual, 'Whether congressional primacy is the singular constitutional requirement or one interpretation among plausible alternatives.').

omega_variable(
    suppression_mechanism_identity,
    'Is suppression of the inherent executive authority claim a structural feature of the law, or an internalized norm that collapses when the executive simply claims the authority and acts?',
    'Historical study of executive unilateral action: when the executive invokes inherent authority and acts, how much does the suppression mechanism constrain or merely produce ex post facto debate? Does suppression persist after the action, or does retroactive authorization reframe it as acceptable?',
    'If suppression is internalized in executive self-restraint, removal of that restraint (a new administration claiming inherent authority boldly) would collapse the constraint''s operation entirely. If suppression is structural (courts, Congress, public opinion actively blocking unilateral action), it persists despite executive claims. Evidence from recent administrations (esp. post-2001) suggests suppression has become increasingly internalized rather than structurally enforced, weakening the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Whether suppression of inherent authority claims is structural enforcement or internalized restraint.').

omega_variable(
    kernel_reading_distinction,
    'This constraint instantiates ONE reading of the contested kernel ''war_powers_allocation.'' The sibling readings—inherent_executive_reading and functional_accommodation_reading—represent structurally distinct interpretations of the same constitutional text. Are these readings genuinely coequal constitutional positions, or does this reading''s axiom (congressional primacy as binding constitutional necessity) foreclose the sibling readings within a single constitutional framework?',
    'Examination of the foundational axioms: congressional_primacy_axiom (Congress must authorize sustained force) directly contradicts inherent_authority_axiom (president has inherent authority to deploy without authorization). Can both be held in one framework? Logically, no—they contradict. But in practice, different institutional actors and constitutional scholars hold each. The question is whether the contradiction is a logical foreclosure or a practical coexistence.',
    'If the readings foreclose each other, the kernel is contested and the winner claims the Constitution. If the readings coexist, the kernel is lived as a political/institutional contest without definitive constitutional resolution. This reading asserts foreclosure (congressional primacy is the binding reading; inherent authority is suppressed). The inherent_executive_reading asserts the opposite. The functional_accommodation_reading avoids the binary by introducing context-dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether this reading''s core axiom forecloses the sibling readings or coexists with them as competing constitutional interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(war__tr_t40, observed).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(war__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(war__be_t40, observed).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(war__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(war__su_t40, observed).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(war__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the war_powers_allocation kernel family. The three stories (congressional_primacy_reading, inherent_executive_reading, functional_accommodation_reading) represent structurally distinct readings of the same constitutional text. Each has a different ε (congressional primacy reading: 0.68, treating unilateral action as extraction; inherent executive reading: lower, treating inherent authority as natural constitutional feature; functional accommodation reading: intermediate, treating context-dependency as legitimate). The three stories share a kernel but diverge on what the kernel requires. Readers should consult all three stories to understand the full contest over war powers allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
