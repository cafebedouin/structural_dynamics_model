% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_feudal_obsolescence, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Feudal Relic — Obsolescence Reading
 *   domain: constitutional/political/historical
 *
 * SUMMARY:
 *   Magna Carta (1215) was a compact negotiated between English barons and
 *   King John to address specific feudal grievances: relief payments,
 *   wardship abuses, scutage, and arbitrary exaction. The feudal-obsolescence
 *   reading holds that this medieval baronial document has no binding
 *   authority over modern sovereignty structures because: (1) the specific
 *   grievances were products of 13th-century feudal tenure and are
 *   structurally obsolete; (2) modern nation-states operate on principles of
 *   indivisible sovereignty that cannot be permanently bound by medieval
 *   compacts; and (3) if Magna Carta were to constrain executive authority,
 *   it would do so parasitically on reinterpretation, not from its original
 *   authority. This reading competes with living-constitutionalist readings
 *   (which hold that Magna Carta's principles have been reinterpreted across
 *   centuries and remain binding) and parliamentary-sovereignty readings
 *   (which hold that Magna Carta's restraints survive only as absorbed into
 *   parliamentary statute). Under the feudal-obsolescence reading, the
 *   constraint operates as a piton: the Charter persists theatrically as a
 *   symbol of rule of law but carries no binding force; its invocation
 *   becomes performative cover for executive discretion.
 *
 * KEY AGENTS:
 *   - executive_authority: Holds sovereign power; benefits from the obsolescence frame by maximizing discretion
 *   - popular_constitutionalism: Doctrine asserting restraint flows from inherited legal principles; loses its historical anchor
 *   - juridical_restraint_doctrine: Legal tradition holding that rule of law requires binding constraints; identity-locked to the restraint claim, severed from Magna Carta by obsolescence
 *   - parliamentary_accountability: Derives legitimacy from representation and accountability; constrained by loss of historical continuity
 *   - historical_jurisprudence: Observer seat; examines whether authority derives from origins or later reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.72).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Feudal Relic — Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional/political/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '3b7c50db-9e5d-4a3b-92c2-9188158f9432').
narrative_ontology:cs_kernel_codification('3b7c50db-9e5d-4a3b-92c2-9188158f9432', fixed_text).
narrative_ontology:cs_authority_grounding('3b7c50db-9e5d-4a3b-92c2-9188158f9432', extraction).
narrative_ontology:cs_interpretation_layer_present('3b7c50db-9e5d-4a3b-92c2-9188158f9432').
narrative_ontology:cs_reading_relation('3b7c50db-9e5d-4a3b-92c2-9188158f9432', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('3b7c50db-9e5d-4a3b-92c2-9188158f9432', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3b7c50db-9e5d-4a3b-92c2-9188158f9432', foundational, feudal_origins_determine_contemporary_irrelevance).
narrative_ontology:cs_axiom_status(feudal_origins_determine_contemporary_irrelevance, holdable).
narrative_ontology:cs_axiom_grounding('3b7c50db-9e5d-4a3b-92c2-9188158f9432', feudal_origins_determine_contemporary_irrelevance, empirically_contingent).
narrative_ontology:cs_axiom('3b7c50db-9e5d-4a3b-92c2-9188158f9432', foundational, sovereignty_indivisible_unbound_by_medieval_compacts).
narrative_ontology:cs_axiom_status(sovereignty_indivisible_unbound_by_medieval_compacts, holdable).
narrative_ontology:cs_axiom_grounding('3b7c50db-9e5d-4a3b-92c2-9188158f9432', sovereignty_indivisible_unbound_by_medieval_compacts, deontological).
narrative_ontology:cs_reference_frame('3b7c50db-9e5d-4a3b-92c2-9188158f9432', modern_indivisible_sovereignty).
narrative_ontology:cs_drift_state('3b7c50db-9e5d-4a3b-92c2-9188158f9432', contemporary_constitutional_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b7c50db-9e5d-4a3b-92c2-9188158f9432', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_authority).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, centralizing_monarchy).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_doctrine).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_accountability).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, feudal_obligations_superseded).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_sovereignty_indivisible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and deploys sovereign power in the modern nation-state. Invokes feudal obsolescence argument to justify executive discretion beyond historical Charter restraints. Argues that Magna Carta addressed 13th-century barons' grievances and has no binding force on contemporary state authority. Maintains the doctrine that modern sovereignty is indivisible and cannot be constrained by a medieval compact.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Movements and doctrines asserting that constitutional restraint flows from popular sovereignty and inherited legal principles (including Magna Carta). Bears the cost of the executive's obsolescence claim because it forecloses the juridical foundation the popular-constitutionalist reading would use to challenge executive overreach. Cannot exit the constraint without adopting an entirely different sovereignty theory.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism, payer,
    moderate, generational, constrained, national).

% Legal and philosophical tradition holding that rule of law requires binding constraints on executive power, with precedent traced through Magna Carta and common law. Pays the cost of the feudal-obsolescence reading by losing its historical grounding — when Magna Carta is declared a dead relic, the juridical restraint doctrine is severed from its most authoritative source. Its advocates are identity-locked: abandoning the restraint doctrine would dissolve their entire intellectual and professional project.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_doctrine, payer,
    powerful, generational, identity_locked, national).

% Parliamentary bodies and accountability mechanisms that derive legitimacy from claims to represent the people's will and to hold executive authority accountable. Bear the cost of the feudal-obsolescence frame because it undermines the historical continuity of chartered restraint — if Magna Carta is a dead feudal document, Parliament's authority to constrain the executive appears less rooted in inherited constitutional law and more contingent on current political power. They are constrained because departing the legitimacy framework would require Parliament to be reconstituted on entirely different grounds.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_accountability, payer,
    organized, biographical, constrained, national).

% The doctrine that sovereign authority cannot be permanently bound by medieval compacts and must remain free to exercise discretion according to contemporary necessity. This is a non-agent doctrinal beneficiary: it collects no rents directly, but its vindication depends on the feudal-obsolescence reading's success.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, monarchical_absolute_authority, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__feudal_obsolescence_reading, monarchical_absolute_authority).

% Scholars and historians examining the actual historical record of Magna Carta's drafting, re-issue, and subsequent legal interpretations. Their analytical position allows them to examine whether the constraint's authority derives from its 13th-century origins or from later readings that invested it with trans-temporal force.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_jurisprudence, observer,
    analytical, generational, analytical, national).

% Jurists and constitutional theorists holding that Magna Carta's principles of lawful restraint have evolved and been reinterpreted across centuries to bind modern authority. Excluded from the feudal-obsolescence reading's framework because their core claim (that inheritance and reinterpretation confer binding force) is directly contradicted by the assertion that feudal origins mean contemporary irrelevance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, living_constitutionalism_advocates, excluded,
    powerful, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_authority).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No real coordination function; the constraint is read as a defunct historical artifact. What Magna Carta once coordinated (baronial grievance redress against feudal abuse) is structurally obsolete — modern nation-states operate on entirely different principles of sovereignty and authority.
% TRANSFER_FUNCTION: Under this reading, no contemporary transfer occurs — Magna Carta is treated as a dead document. The extraction occurs at a higher level: the feudal-obsolescence frame transfers legitimacy FROM historical restraint doctrines TO executive discretion, allowing unconstrained executive authority to persist without the friction of appealing to a binding constitutional precedent.
% ABSENT_VOICES: Living constitutionalism advocates and popular constitutionalist scholars are excluded: their voice — that historical precedent carries binding force through reinterpretation and inheritance — directly contradicts the obsolescence premise. Parliamentary accountability bodies are present but constrained from deploying their historical legitimacy claim.
% DISAPPEARANCE_RATIONALE: If this reading (feudal obsolescence) disappeared and living-constitutionalist readings prevailed, the world would substantially rearrange: executive authority would face renewed constraints justified by Magna Carta's binding force, judicial review would shift to demand historical rooting in inherited law, and parliamentary accountability would be reinvigorated by appeal to 800-year-old chartered limits. If the reading persists, the world stays organized around executive discretion. The contest is over whether such historical anchors bind modern authority.
% FOUNDING_PROBLEM: Medieval barons faced arbitrary feudal exaction and sought redress from their overlord, King John. Magna Carta addressed the specific grievances of a feudal nobility operating under 13th-century property and military obligation systems that no longer exist.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars outside the executive-authority seat confirm: the specific grievances Magna Carta addressed (feudal incidents, wardship abuses, relief payments, scutage) are structurally obsolete in modern nation-states operating on different revenue and authority systems. Even scholars who affirm Magna Carta's modern binding force concede that its original feudal referents are gone.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects high structural benefit to executive authority: the feudal-obsolescence reading allows unconstrained discretion by declaring the historical restraint dead. Suppression is high (0.72) because maintaining the reading requires actively suppressing countervailing readings — popular constitutionalists and juridical restraint theorists must be excluded from authoritative venues, their arguments must be dismissed as clinging to a dead past. Theater ratio at 0.58 indicates substantial performative content: Magna Carta is invoked ceremonially (in judicial oaths, constitutional preambles, historical pageantry) while its restraining force is simultaneously denied. The measurement series shows extraction accumulating over the interval (0.32 → 0.68): as the feudal-obsolescence reading consolidates, executive discretion expands unopposed, extraction intensifies. Theater initially high because the performative invocation precedes the acknowledgment of obsolescence; as the reading consolidates, theater stabilizes near 0.58 — the constraint becomes a settled theatrical relic. Suppression rises sharply early (0.55 → 0.72 by midpoint), tracking the effort required to exclude and discredit competing readings; it plateaus at 0.72 once the reading is normalized and dissent is marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (juridical restraint, popular constitutionalism, parliamentary accountability) and the beneficiary seat (executive authority) should compute radically different types. From the executive's view: no constraint operates, Magna Carta is dead, pure beneficiary position, no directionality. From the juridical-restraint view: a snare operates — the constraint frames the Charter as dead specifically to suppress the restraint doctrine's historical grounding, extraction flows upward to executive discretion, high targeting. The gap is the intentionality of the obsolescence frame: is it a neutral discovery that feudal documents don't bind modern states, or is it a strategically deployed reading designed to suppress alternative authority claims? The structural data (high suppression, accumulating extraction, high theater) support the second reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive authority benefits from the obsolescence reading — it maximizes discretion, eliminates historical restraint claims, and captures the legitimacy transfer. Directionality is low (d near 0.0 for benefits, near 1.0 for targets). Popular constitutionalism, juridical restraint doctrine, and parliamentary accountability all pay the cost of losing their historical anchor and having their authority claims delegitimized. Their directionality is high (d near 1.0) — they are the targets. The feudal-obsolescence frame transfers legitimacy from restraint doctrines to executive discretion, which is the asymmetric extraction. Exit options for the victims are severely constrained: juridical restraint doctrine cannot abandon the rule-of-law claim without dissolving itself (identity_locked); parliamentary accountability cannot depart the legitimacy framework without being reconstituted (constrained); popular constitutionalism cannot exit without adopting a fundamentally different sovereignty theory (identity_locked). The reading depends on this exit suppression: if juridical restraint advocates could simply switch to living-constitutionalism, the obsolescence frame collapses. The engine amplifies d values toward targets because the exit options are tight.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits severe mandatrophy: the founding problem (baronial grievance redress in 13th-century feudal tenure) is demonstrably dead. Yet the constraint persists because the feudal-obsolescence reading repurposes the dead mandate into cover for executive discretion. A living-constitutionalist reading would resurrect the mandate by arguing that Magna Carta's principles of lawful restraint have evolved to bind modern authority — but the feudal-obsolescence reading precludes this reanimation by asserting that origins determine contemporary irrelevance. The mandatrophy is the point: the constraint survives not because its original purpose is satisfied or because participants choose to renew it, but because actors benefit from the obsolescence frame. The theater ratio (0.58) indicates that a substantial share of effort goes to maintaining the symbolic status of Magna Carta — ceremonial invocation, pageantry, historical reverence — while the restraining force is simultaneously denied. This is the classic piton signature: the constraint persists by inertia and symbolic performance, not by functional necessity or participant agreement. No party benefits enough to rebuild the original mandate; no party is hurt enough to fix the constraint entirely. The executive benefits from the obsolescence frame (discretion unconstrained), but not so much that it would risk abolishing Magna Carta symbolically and inviting backlash. The juridical restraint doctrine is hurt (severed from historical anchor), but it lacks the power to force a reckoning — its advocates are intellectuals and jurists, not a mobilizable constituency. Mandatrophy is resolved by recognizing that the constraint is dead as a real restraint and persists as a theatrical relic that enables extraction by denying its own force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_obsolescence_vs_constitutional_inheritance,
    'Is the feudal obsolescence of Magna Carta''s specific grievances sufficient to render its entire restraining principle obsolete, or can constitutional principles survive the obsolescence of their original context?',
    'Jurisprudential analysis of how other constitutional principles (due process, limited government, separation of powers) that originated in particular historical contexts have been reinterpreted and applied across radically different social organizations. Comparative legal study of how other legal systems treat historically-grounded constitutional restraints.',
    'If constitutional principles can survive their original context via reinterpretation, the feudal-obsolescence reading is undermined and living-constitutionalism gains force. If the reading is correct, then restraint doctrines derived from medieval compacts have no authority over modern states, and executive discretion is unconstrained by Magna Carta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_obsolescence_vs_constitutional_inheritance, conceptual, 'Whether constitutional principles are context-bound or context-transcendent.').

omega_variable(
    intentionality_of_obsolescence_framing,
    'Is the feudal-obsolescence reading a neutral historical discovery, or is it a strategically deployed frame designed to suppress living-constitutionalist and popular-constitutionalist readings that would constrain executive authority?',
    'Historical analysis of when the obsolescence frame emerged, who deployed it, and what institutional interests it served. Study of whether the frame appears in periods of executive consolidation or judicial activism in restraint doctrine. Examination of whether alternative readings were suppressed or discredited alongside the frame''s promotion.',
    'If the reading is strategic, it is a snare (pure extraction using an obsolescence argument as cover). If it is neutral, it is a piton (genuine doctrinal death persisting theatrically). If reading classification depends on whether the frame is strategic, the intentionality question determines the constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_obsolescence_framing, empirical, 'Whether the feudal-obsolescence frame is a neutral discovery or a strategic deployment.').

omega_variable(
    binding_authority_criterion,
    'What makes a historical legal document binding on modern authority? Is it origin (the document was legitimately established in its time), reinterpretation (the document''s principles have been continuously reinterpreted across time), or contemporary choice (modern actors choose to treat it as binding)?',
    'Philosophical and legal analysis of legitimacy and authority: survey jurisprudential traditions across common law, civil law, and constitutional theory on how binding authority is grounded. Empirical study of which criterion parties actually use when they invoke or refuse historical documents.',
    'Different criteria yield different classifications of Magna Carta''s authority and different verdict on whether the feudal-obsolescence reading is correct. If origin is the criterion, the reading is correct (feudal origins mean dead authority). If reinterpretation is the criterion, living-constitutionalism prevails. If contemporary choice is the criterion, parliamentary sovereignty prevails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binding_authority_criterion, preference, 'What criterion grounds binding constitutional authority across time.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) structural (external barriers to deploying living-constitutionalist arguments in authoritative venues) or internalized (advocates of juridical restraint have adopted the obsolescence frame and no longer deploy their own historical arguments)?',
    'Post-suppression trajectory: if the feudal-obsolescence frame is removed (via legislative repeal of the frame''s institutional codification, or via regime change that elevates living-constitutionalism), do juridical restraint advocates immediately re-deploy their historical arguments, or has suppression become so internalized that they continue to doubt the force of Magna Carta even when external barriers are removed?',
    'If structural, the suppression can be lifted by removing institutional barriers (change in judicial appointment, legislative override). If internalized, juridical restraint advocates carry the suppression with them even after the frame is formally abandoned; intellectual and professional reconstruction would be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of juridical restraint doctrine is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_feudal_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(mc_feudal_tr_t0, observed).
narrative_ontology:measurement(mc_feudal_tr_t5, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement_basis(mc_feudal_tr_t5, observed).
narrative_ontology:measurement(mc_feudal_tr_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(mc_feudal_tr_t10, observed).
narrative_ontology:measurement(mc_feudal_tr_t15, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(mc_feudal_tr_t15, observed).
narrative_ontology:measurement(mc_feudal_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(mc_feudal_tr_t20, observed).
narrative_ontology:measurement(mc_feudal_tr_t25, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(mc_feudal_tr_t25, observed).
narrative_ontology:measurement(mc_feudal_tr_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(mc_feudal_tr_t30, observed).
narrative_ontology:measurement(mc_feudal_tr_t35, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement_basis(mc_feudal_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(mc_feudal_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(mc_feudal_be_t0, observed).
narrative_ontology:measurement(mc_feudal_be_t5, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(mc_feudal_be_t5, observed).
narrative_ontology:measurement(mc_feudal_be_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(mc_feudal_be_t10, observed).
narrative_ontology:measurement(mc_feudal_be_t15, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(mc_feudal_be_t15, observed).
narrative_ontology:measurement(mc_feudal_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(mc_feudal_be_t20, observed).
narrative_ontology:measurement(mc_feudal_be_t25, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(mc_feudal_be_t25, observed).
narrative_ontology:measurement(mc_feudal_be_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(mc_feudal_be_t30, observed).
narrative_ontology:measurement(mc_feudal_be_t35, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(mc_feudal_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc_feudal_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(mc_feudal_su_t0, observed).
narrative_ontology:measurement(mc_feudal_su_t5, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(mc_feudal_su_t5, observed).
narrative_ontology:measurement(mc_feudal_su_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(mc_feudal_su_t10, observed).
narrative_ontology:measurement(mc_feudal_su_t15, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(mc_feudal_su_t15, observed).
narrative_ontology:measurement(mc_feudal_su_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(mc_feudal_su_t20, observed).
narrative_ontology:measurement(mc_feudal_su_t25, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(mc_feudal_su_t25, observed).
narrative_ontology:measurement(mc_feudal_su_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(mc_feudal_su_t30, observed).
narrative_ontology:measurement(mc_feudal_su_t35, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(mc_feudal_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.08).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'magna_carta_constraint_authority'. The feudal-obsolescence reading is characterized by treating Magna Carta as a dead feudal document with no binding force on modern sovereignty (high extractiveness, high suppression, high theater ratio). The living-constitutionalism reading (sibling) treats Magna Carta's principles as reinterpreted across centuries and binding on modern authority (expected: lower extractiveness, lower suppression, lower theater). The parliamentary-sovereignty reading (sibling) treats Magna Carta's restraints as surviving only through parliamentary absorption and subject to parliamentary revision (expected: moderate extractiveness, moderate suppression, moderate theater). All three share the same kernel (the persisting commitment to Magna Carta's authority) but instantiate different constraint types because different parties hold different readings. The ε values differ sharply because the readings assert fundamentally different claims about what Magna Carta constrains in the modern era.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
