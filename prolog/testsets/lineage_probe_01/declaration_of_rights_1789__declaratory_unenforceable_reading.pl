% ============================================================================
% CONSTRAINT STORY: declaration_of_rights_1789__declaratory_unenforceable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_declaration_of_rights_1789__declaratory_unenforceable_reading, []).

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
 *   constraint_id: declaration_of_rights_1789__declaratory_unenforceable_reading
 *   human_readable: Declaration of Rights (1789): Declaratory Unenforceable Reading
 *   domain: legal/doctrinal/constitutional_history
 *
 * SUMMARY:
 *   The Declaration of Rights (1789) declared universal principles of human
 *   rights ('all men are born free and equal in rights') while the French
 *   Constitution it accompanied restricted political rights to
 *   property-owning males, excluded women, and suppressed remedies for
 *   claimants harmed by these restrictions. This constraint, as read through
 *   the declaratory-unenforceable lens, is not a natural feature of
 *   constitutional law but a specific institutional arrangement: the
 *   Declaration establishes a canonical text of universal principles, but the
 *   Constitution and the courts that interpret it refuse to enforce those
 *   principles against violations of the franchise. No court hears claims
 *   based on the Declaration alone; no remedy for rights violation exists
 *   outside the property-franchise system; no review mechanism permits
 *   constitutional challenge to the franchise itself. The Declaration thus
 *   functions as a rhetorical asset for the regime (it claims moral
 *   authority, attracts international support, provides legitimate cover for
 *   revolutionary ambitions) while simultaneously suppressing the actual
 *   remedy mechanisms that would make those principles enforceable against
 *   the franchise restrictions. The extractiveness grows over time (from 0.35
 *   at proclamation to 0.58 by the 1791 Constitution's implementation) as the
 *   suppression becomes crystallized in constitutional silence. The theater
 *   ratio rises (from 0.50 to 0.68) as the Declaration's ceremonial
 *   invocation increases while its legal force remains dormant—the
 *   performance of universal principles becomes decoupled from their
 *   enforceability. The suppression requirement rises (from 0.65 to 0.72) as
 *   the regime must actively prevent courts from entertaining
 *   Declaration-based claims and actively manage the cognitive dissonance
 *   between universal declaration and franchise restriction.
 *
 * KEY AGENTS:
 *   - Revolutionary Assembly: Institutional beneficiary (institutional/arbitrage) — benefits from Declaration's rhetorical career and international legitimacy while preserving property-based franchise
 *   - Disenfranchised Claimants (women, non-property-owners, workers): Primary victims (powerless/trapped) — declare rights exist but no forum for remedy; trapped by suppression through institutional omission
 *   - Provincial Magistrates & Judiciary: Secondary beneficiary/enforcer (institutional/constrained) — coordinate local justice using Declaration's legitimacy while suppressing claims that would challenge the franchise they enforce
 *   - Transnational Abolitionist Networks: Organized challengers (organized/mobile) — leverage Declaration's universal language to create pressure against national suppression; provide exit path by internationalizing the rights discourse
 *   - Conservative Jurists (post-1848): Institutional observers (institutional/arbitrage) — see Declaration as historical artifact, ceremonially invoked but functionally degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(declaration_of_rights_1789__declaratory_unenforceable_reading, 0.58).
domain_priors:suppression_score(declaration_of_rights_1789__declaratory_unenforceable_reading, 0.72).
domain_priors:theater_ratio(declaration_of_rights_1789__declaratory_unenforceable_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(declaration_of_rights_1789__declaratory_unenforceable_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(declaration_of_rights_1789__declaratory_unenforceable_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(declaration_of_rights_1789__declaratory_unenforceable_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(declaration_of_rights_1789__declaratory_unenforceable_reading, tangled_rope).
narrative_ontology:human_readable(declaration_of_rights_1789__declaratory_unenforceable_reading, "Declaration of Rights (1789): Declaratory Unenforceable Reading").
narrative_ontology:topic_domain(declaration_of_rights_1789__declaratory_unenforceable_reading, "legal/doctrinal/constitutional_history").

domain_priors:requires_active_enforcement(declaration_of_rights_1789__declaratory_unenforceable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(declaration_of_rights_1789__declaratory_unenforceable_reading, '8042af07-d422-454c-8021-48b2720f1c81').
narrative_ontology:cs_kernel_codification('8042af07-d422-454c-8021-48b2720f1c81', formalized).
narrative_ontology:cs_authority_grounding('8042af07-d422-454c-8021-48b2720f1c81', extraction).
narrative_ontology:cs_interpretation_layer_present('8042af07-d422-454c-8021-48b2720f1c81').
narrative_ontology:cs_reading_relation('8042af07-d422-454c-8021-48b2720f1c81', declaration_of_rights_1789__bourgeois_property_charter_reading, coexists_with).
narrative_ontology:cs_reading_relation('8042af07-d422-454c-8021-48b2720f1c81', declaration_of_rights_1789__universal_charter_reading, influences).
narrative_ontology:cs_axiom('8042af07-d422-454c-8021-48b2720f1c81', foundational, enforcement_via_institutional_omission).
narrative_ontology:cs_axiom_status(enforcement_via_institutional_omission, holdable).
narrative_ontology:cs_axiom_grounding('8042af07-d422-454c-8021-48b2720f1c81', enforcement_via_institutional_omission, empirically_contingent).
narrative_ontology:cs_axiom('8042af07-d422-454c-8021-48b2720f1c81', foundational, suppression_through_institutional_silence).
narrative_ontology:cs_axiom_status(suppression_through_institutional_silence, holdable).
narrative_ontology:cs_axiom_grounding('8042af07-d422-454c-8021-48b2720f1c81', suppression_through_institutional_silence, empirically_contingent).
narrative_ontology:cs_reference_frame('8042af07-d422-454c-8021-48b2720f1c81', universal_principles_enforceable_norm).
narrative_ontology:cs_drift_state('8042af07-d422-454c-8021-48b2720f1c81', constitution_implementation_1791, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8042af07-d422-454c-8021-48b2720f1c81', '').
narrative_ontology:cs_kernel_id(declaration_of_rights_1789__declaratory_unenforceable_reading, declaration_of_rights_1789).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(declaration_of_rights_1789__declaratory_unenforceable_reading, revolutionary_rhetoric).
narrative_ontology:constraint_victim(declaration_of_rights_1789__declaratory_unenforceable_reading, claimants_without_forum).
narrative_ontology:constraint_victim(declaration_of_rights_1789__declaratory_unenforceable_reading, constitutional_enforcement_gap).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED CLAIMANT (SNARE) — Declared rights exist; no court will hear claims; no remedy available; no review mechanism for violations. The claimant faces the full extraction of gap between declared and delivered with no exit. Declaration creates the false impression of protection while suppressing actual remedy mechanisms. Maximum extraction from a powerless position.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROVINCIAL MAGISTRATE (TANGLED ROPE) — Coordinates local justice administration using Declaration principles (coordination function) while suppressing claims that would challenge the franchise restrictions the magistrate enforces. Genuine coordination of judicial function with embedded extraction: the magistrate benefits from Declaration's legitimacy while enforcing the suppression that makes it unenforceable.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REVOLUTIONARY ASSEMBLY (ROPE) — Benefits from Declaration's rhetorical career (coordinates revolutionary legitimacy internationally, establishes principles that justify the regime, creates narrative of enlightenment). The Assembly experiences the constraint as pure coordination: declaring rights in universal language enables the regime to claim moral authority while preserving the franchise restrictions that protect revolutionary property gains. Net beneficiary with arbitrage capacity.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSNATIONAL ABOLITIONIST NETWORK (SCAFFOLD) — Organized agents (British abolitionists, French abolitionists-in-exile) leverage the Declaration's universal language ('rights of man, not of Frenchmen') as a doctrinal weapon against slavery and franchise restrictions. This reading creates structural pressure on the unenforceable suppression: if the Declaration truly speaks for mankind, claimants can cite it across borders, bypassing national enforcement silence. Sunset: as the Declaration's universal reach is weaponized, the suppression by national omission becomes untenable — the constraint has a built-in timer.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONSERVATIVE JURIST (PITON) — From a post-1848 perspective, the Declaration is treated as a historical artifact, ceremonially invoked but functionally replaced by a positive Constitution with enforcement mechanisms. The Declaration's declaratory status is performative nostalgia: courts cite it for rhetorical authority while denying it creates enforceable claims. The mechanism persists through inertia — maintained because its symbolic weight still matters, not because it actually governs.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the gap between declared principles and enforced law is an immutable property of how constitutions work: all declaratory statements lack enforcement mechanisms until codified in positive law. This perspective sees the Declaration's unenforceability as an inherent feature of the distinction between principles and rules. However, the structural data (beneficiary: revolutionary rhetoric; victim: claimants without forum; suppression by deliberate omission) reveals this as a false summit — the unenforceability is not a law of nature but a political choice to declare without providing remedy.
constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(declaration_of_rights_1789__declaratory_unenforceable_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(declaration_of_rights_1789__declaratory_unenforceable_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(declaration_of_rights_1789__declaratory_unenforceable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(declaration_of_rights_1789__declaratory_unenforceable_reading, TR),
    TR >= 0.70.

:- end_tests(declaration_of_rights_1789__declaratory_unenforceable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The extraction mechanism is the gap between declared rights and denied remedies. Claimants are promised universal principles but denied access to any forum for claim-making. The regime extracts legitimacy benefit (international authority, rhetorical cover for revolutionary consolidation) while extracting enforcement cost from claimants (they face the psychological and material burden of rights that exist but cannot be claimed). The value rises over the interval (0.35 → 0.58) because the extraction mechanism becomes institutionalized: initially the Declaration might be read as aspirational (low extraction), but as the Constitution crystallizes the suppression in legal silence, the gap becomes structural. Suppression (0.72): High. The suppression is not external barrier (physical confinement) but institutional omission: courts are not granted jurisdiction over Declaration claims; no remedy mechanism is created; no review process exists. The suppression is actively maintained through procedural silence and interpretive doctrine ('the Declaration is not self-executing'). Theater ratio (0.68): Moderate-high. The Declaration serves partly as genuine aspiration for the regime itself and partly as ceremonial legitimation. The regime uses the Declaration's language as moral authority but maintains operative silence in the courts. By the 1791 Constitution, the Declaration becomes theatrical: invoked for legitimacy, emptied of enforcement content, maintained through inertia because rejecting it would undermine the regime's self-image.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The revolutionary Assembly sees Rope—pure coordination of legitimate principles establishing regime authority. The disenfranchised claimant sees Snare—principles exist but no forum for remedy, trapped by institutional omission. The provincial magistrate sees Tangled Rope—genuine coordination of justice (declaring rights, establishing rule of law norms) layered with extraction (enforcing the franchise restrictions that suppress remedy). The transnational network sees Scaffold—a temporary suppression destined for sunset as universal language is weaponized to create alternative (international) forums for rights claims. The conservative jurist sees Piton—a ceremonial text maintained by inertia, functionally replaced by positive constitutional law. The analytical observer risks seeing Mountain—treating the gap between declaration and enforcement as an immutable feature of how constitutional law works—but the structural data reveals this as false summit: the gap is sustained by deliberate institutional choices (absence of court jurisdiction, absence of remedy mechanism), not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position relative to the constraint. The Assembly (beneficiary + arbitrage exit) gets low d → negative χ (benefits from the constraint). Disenfranchised claimants (victim + trapped exit) get high d → high χ (maximum experienced extraction). Magistrates (mixed: coordinate justice + enforce suppression) get moderate d with asymmetry between their institutional power and their functional role. The transnational network (organized + mobile) has moderate-to-low d because their exit option (international forum) is available and they have power to exploit it. The conservative jurist (institutional + arbitrage) experiences low d (beneficiary of the degraded stability). The analytical observer's d is derived from position as observer rather than stakeholder—0.72 canonical, producing the Mountain classification that the false summit detector will challenge.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how a single structural configuration (declared principles + denied remedies) instantiates multiple legitimate classification types depending on the observer's position. The mandatrophy is not 'which type is correct' but 'what is the constraint from each perspective?' This is the diagnostic exemplar of what happens when a regime announces universal principles while maintaining particularist restrictions: Snare to the powerless (principles without remedy), Rope to the beneficiary (coordination of regime legitimacy), Tangled Rope to the enforcer (mixed function), Scaffold to the organized challenger (temporary suppression vulnerable to internationalization), Piton to the post-hoc observer (ceremonial inertia), Mountain to the analytical observer risking naturalization. The false summit detector identifies the Mountain as naturalization: the gap is not a law of nature but a political structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaration_codification_deliberateness,
    'Is the Declaration''s lack of enforcement mechanism a structural inevitability of declaratory texts, or a deliberate political choice by the Assembly to preserve the franchise restrictions?',
    'Historical archive analysis: Assembly debates on enforcement mechanisms; comparative analysis of earlier declarations (1776 Virginia Declaration of Rights, 1683 English Bill of Rights) that DID include enforcement or remedy provisions; textual analysis of why Article 16 on separation of powers omits a declaration-enforcement power',
    'If deliberate choice: classification as Snare/Tangled Rope confirmed (extraction mechanism is suppression by design). If structural inevitability: reclassifies toward Rope or Scaffold (gap is coordination problem, not extraction). If mixed (deliberate + structural): confirms Tangled Rope reading (genuine coordination function layered with intentional suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(declaration_codification_deliberateness, empirical, 'Whether unenforceability is deliberate political choice or structural inevitability of declarations').

omega_variable(
    franchise_restriction_alignment,
    'To what extent does the Declaration''s scope (universal principles) deliberately misalign with its enforcement arena (the French Constitution''s restricted franchise)?',
    'Textual comparison: which Declaration articles would conflict with the 1791 Constitution''s property-based franchise if actually enforced; identification of deliberately avoided conflict zones in the Assembly''s framing; analysis of whether the gap creates plausible deniability (''the Declaration doesn''t actually conflict—you''re misreading it'') vs explicit conflict suppression',
    'If deliberate misalignment: beneficiary is the regime''s capacity to claim universal principles while maintaining property-based exclusions. If unintentional: constrains the ''extraction'' interpretation — the gap becomes a design flaw rather than a mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchise_restriction_alignment, empirical, 'Deliberate vs accidental misalignment between Declaration''s scope and Constitutional franchise').

omega_variable(
    reading_contest_foreclosure,
    'Does the declaratory-unenforceable reading''s core claim (suppression by omission of enforcement) logically foreclose the bourgeois-property-charter reading (Declaration as sacred charter for property), or do both readings coexist within different instantiations of the revolutionary framework?',
    'Logical analysis: can a document simultaneously be ''a sacred inviolable charter for property'' (bourgeois reading) AND ''a declaration without enforcement mechanism that suppresses non-property-holders'' (declaratory-unenforceable reading)? Answer: yes, they coexist when property-holders and non-property-holders inhabit the same legal framework but with asymmetric access to remedy. The readings do NOT foreclose each other; they describe different structural positions.',
    'If foreclosure: the readings are genuinely incompatible; one reading''s triumph makes the other impossible. If coexistence: both readings remain live, held simultaneously by different stakeholders, revealing the Declaration''s structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether declaratory-unenforceable reading forecloses bourgeois-property-charter reading or coexists with it').

omega_variable(
    enforcement_gap_extractiveness_boundary,
    'Where is the boundary between the Declaration''s unenforceability as a coordination cost (legitimate gap between aspiration and current capacity, ε ≈ 0.15) vs unenforceability as an extraction mechanism (gap sustained by deliberate suppression, ε ≈ 0.58)?',
    'Counterfactual: if the Assembly had established a minimal enforcement mechanism (e.g., a registry of violations, a petition process with mandatory response, an appeals path), would the Declaration have functioned identically? If yes: the gap is structural and ε ≈ 0.15. If no: the suppression is deliberate and ε ≈ 0.58. Historical comparison: did other 18th-century declarations function effectively with minimal enforcement? This test disambiguates structural inevitability from political choice.',
    'Classification sensitivity: low ε → Rope; high ε → Tangled Rope. Current assessment of ε = 0.58 rests on the hypothesis of deliberate suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_extractiveness_boundary, empirical, 'Boundary between coordination cost and extraction mechanism in Declaration''s enforcement gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(declaration_of_rights_1789__declaratory_unenforceable_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decl_unf_theater_t0, declaration_of_rights_1789__declaratory_unenforceable_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(decl_unf_theater_t2, declaration_of_rights_1789__declaratory_unenforceable_reading, theater_ratio, 2, 0.62).
narrative_ontology:measurement(decl_unf_theater_t5, declaration_of_rights_1789__declaratory_unenforceable_reading, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(decl_unf_extract_t0, declaration_of_rights_1789__declaratory_unenforceable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(decl_unf_extract_t2, declaration_of_rights_1789__declaratory_unenforceable_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(decl_unf_extract_t5, declaration_of_rights_1789__declaratory_unenforceable_reading, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(decl_unf_suppress_t0, declaration_of_rights_1789__declaratory_unenforceable_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(decl_unf_suppress_t2, declaration_of_rights_1789__declaratory_unenforceable_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(decl_unf_suppress_t5, declaration_of_rights_1789__declaratory_unenforceable_reading, suppression_requirement, 5, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(declaration_of_rights_1789__declaratory_unenforceable_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(declaration_of_rights_1789__declaratory_unenforceable_reading, declaration_of_rights_1789__bourgeois_property_charter_reading).
narrative_ontology:affects_constraint(declaration_of_rights_1789__declaratory_unenforceable_reading, declaration_of_rights_1789__universal_charter_reading).
narrative_ontology:affects_constraint(declaration_of_rights_1789__declaratory_unenforceable_reading, french_constitutional_franchise_restriction_1791).
narrative_ontology:affects_constraint(declaration_of_rights_1789__declaratory_unenforceable_reading, judicial_non_enforceability_of_declarations).

% DUAL FORMULATION NOTE:
% The Declaration of Rights is a contested kernel with three distinct structural readings. This file models the declaratory-unenforceable reading: the Declaration as a principled announcement without enforcement mechanisms, suppressing remedy access while providing rhetorical benefit to the regime. Sibling readings model the same kernel text under different interpretive frames: the bourgeois-property-charter reading emphasizes Article 17's protection of property as the Declaration's core; the universal-charter reading emphasizes the text's address to 'all mankind' beyond French borders. All three readings share the base text but produce different ε, different beneficiary/victim sets, and different structural classifications. The network links this reading to its siblings and to downstream constraints that inherit the enforcement-gap structure (the 1791 Constitution's codification, the judicial doctrine of non-self-executing declarations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
