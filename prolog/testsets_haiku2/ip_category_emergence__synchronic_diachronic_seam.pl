% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic-Diachronic Seam (M4/M5 Collapse Test)
 *   domain: legal/intellectual-property/jurisprudence
 *
 * SUMMARY:
 *   This reading of the IP category emergence kernel tests whether
 *   thinkability (the coherence of expression as a legally ownable concept)
 *   and first-holding (the moment when statutory proprietors replace
 *   customary practitioners as occupancy-holders) are formally independent or
 *   always co-occur. The 1710 Statute of Anne marks both a conceptual
 *   reorganization (expression becomes thinkable as transferable property)
 *   and an occupancy shift (authors and licensees replace guild and crown
 *   privilege). The constraint under this reading is the doctrinal claim that
 *   BOTH changes are necessary and co-constitutional. Sibling readings
 *   isolate one or the other (thinkability_reading emphasizes conceptual
 *   emergence; first_holding_reading emphasizes occupancy transfer). This
 *   reading's M4/M5 collapse test asks: if the two changes can vary
 *   independently, the kernel structure is authentic; if they always
 *   co-occur, the distinction is a temporal framing artifact with no real
 *   basis.
 *
 * KEY AGENTS:
 *   - statutory_copyright_proprietors: organized beneficiaries collecting licensing revenue (1710 onward)
 *   - prior_common_law_users: powerful payers losing occupancy claim without compensation
 *   - customary_reproduction_practitioners: moderate-power payers whose craft is re-characterized as infringement
 *   - legal_doctrine_builders: institutional agenda-setters choosing between thinkability-primary and occupancy-primary readings
 *   - contesting_philosophers: analytical observers testing whether independence holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.62).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.41).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic-Diachronic Seam (M4/M5 Collapse Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal/intellectual-property/jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '921b677a-730b-4f75-98d1-63983bf24e70').
narrative_ontology:cs_kernel_codification('921b677a-730b-4f75-98d1-63983bf24e70', fixed_text).
narrative_ontology:cs_authority_grounding('921b677a-730b-4f75-98d1-63983bf24e70', lineage).
narrative_ontology:cs_interpretation_layer_present('921b677a-730b-4f75-98d1-63983bf24e70').
narrative_ontology:cs_reading_relation('921b677a-730b-4f75-98d1-63983bf24e70', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('921b677a-730b-4f75-98d1-63983bf24e70', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('921b677a-730b-4f75-98d1-63983bf24e70', foundational, thinkability_occupancy_co_constitution).
narrative_ontology:cs_axiom_status(thinkability_occupancy_co_constitution, holdable).
narrative_ontology:cs_axiom_grounding('921b677a-730b-4f75-98d1-63983bf24e70', thinkability_occupancy_co_constitution, empirically_contingent).
narrative_ontology:cs_axiom('921b677a-730b-4f75-98d1-63983bf24e70', secondary, independence_test_diagnostic).
narrative_ontology:cs_axiom_status(independence_test_diagnostic, holdable).
narrative_ontology:cs_axiom_grounding('921b677a-730b-4f75-98d1-63983bf24e70', independence_test_diagnostic, deontological).
narrative_ontology:cs_reference_frame('921b677a-730b-4f75-98d1-63983bf24e70', statute_of_anne_unified_regime).
narrative_ontology:cs_drift_state('921b677a-730b-4f75-98d1-63983bf24e70', contemporary_independent_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('921b677a-730b-4f75-98d1-63983bf24e70', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, statutory_copyright_proprietors).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, prior_common_law_users).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, customary_reproduction_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Beginning 1710 (UK Statute of Anne), gain exclusive right to copy and distribute literary expression. They benefit from the statutory category that marks authorship as a property right. Their interest depends on the category remaining both coherent (thinkable as a legal object) and exclusive (occupancy stable against common-law claims). They enforce statutory boundaries against unlicensed reproduction.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, statutory_copyright_proprietors, beneficiary,
    organized, generational, constrained, national).

% Before 1710, reproduced texts under guild custom, royal privilege, or common inheritance norms. Statutory copyright displaces their occupancy claim. They bear the loss of customary right to reproduce and distribute. Their exit is blocked by the category's enforced coherence: to exit, the category would have to dissolve, but their own prior claim to authorship rested on the same texts being reproducible.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, prior_common_law_users, payer,
    powerful, biographical, identity_locked, national).

% Print shops, scribes, and manuscript reproducers who operated under guild rules and local custom. Statutory copyright re-characterizes their craft as infringement. They must either license from proprietors or cease. Their alternatives are truncated by the category's enforcement: they cannot argue the category is invalid without losing all claim to craft legitimacy.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, customary_reproduction_practitioners, payer,
    moderate, biographical, trapped, national).

% Judges, lawmakers, and jurists who construct and defend the statutory category. They decide whether the category is primarily about thinkability (coherence of expression as an ownable thing) or occupancy (transfer of reproduction rights from custom to statute). The category's persistence depends on their interpretive choices.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_doctrine_builders, agenda_setter,
    institutional, generational, analytical, national).

% Intellectual historians and legal philosophers who ask: did the category emerge because expression became thinkable as property (a conceptual breakthrough requiring no occupancy change), or did thinkability and occupancy change always co-occur, making the apparent distinction a temporal framing artifact with no real structural independence?
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, contesting_philosophers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, statutory_copyright_proprietors).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes literary expression as a fungible, transferable legal category: authors, printers, and distributors can claim and license reproduction rights under a single, coherent regime rather than competing under guild custom, royal privilege, or common inheritance claims.
% TRANSFER_FUNCTION: Moves the right to reproduce and distribute literary works from customary reproduction practitioners and common-law users to statutory proprietors (publishers, authors with assigned rights). The proprietors collect licensing revenue from reproduction that would have flowed through guild or custom channels.
% ABSENT_VOICES: Customary practitioners' objections that their craft was legitimate under prior norms and that the statute forcibly displaces them without compensation; prior common-law claim-holders (authors who benefited from uncompensated circulation and manuscript inheritance) who would argue the category change redistributes occupancy without establishing genuine thinkability gain.
% DISAPPEARANCE_RATIONALE: The legal doctrine builders argue: if the category disappeared, authors would lose property incentives and expression would revert to custom/privilege. The contesting philosophers argue: if the category is a temporal framing artifact (thinkability and occupancy always co-occur), then disappearance of the statutory form would reorganize occupancy but not change what is conceptually thinkable as property — the dissolution is real but the thinkability claim was spurious. If thinkability and occupancy are formally independent, then disappearance would dissolve occupancy but leave expression thinkable as property in some other form.
% FOUNDING_PROBLEM: Before 1710, reproduction of literary works was governed by guild privilege, royal monopoly, and common inheritance: no unified regime, no stable claim to authorship, no predictable licensing path. The Statute of Anne unified these into a single property category.
% FOUNDING_PROBLEM_CORROBORATION: Statutory advocates (legal doctrine builders) attest the founding problem is the incoherence of prior regimes and the solution is category unification. Contesting philosophers (external to the doctrine) attest the problem was real but the solution conflates two distinct gains: making expression thinkable as property (a conceptual claim) and transferring occupancy to statute (an institutional claim). The philosophers argue these could be independent; statutory advocates argue they always co-occur, making the distinction vacuous.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness jumps sharply at 1710 (0.15 → 0.52) when the statute transfers occupancy from custom to proprietors; it then levels off (0.52 → 0.62 by 1800), indicating the extraction becomes stable once the category is enforced. Suppression parallels this trajectory: minimal before statute (guild custom was less coercive), then required to maintain the category once proprietary claims clash with customary practice. Theater ratio rises post-statute as the doctrine increasingly emphasizes the conceptual coherence (thinkability) of the category to justify suppression of customary claims — what was institutional coercion becomes presented as logical necessity. The shared time grid captures the synchronized emergence of all three metrics at 1710, the point when both category emergence and occupancy transfer occurred; by 1800 they are inseparable, leaving the independence question empirically ambiguous.
 *
 * PERSPECTIVAL GAP:
 *   From the legal doctrine builders' seat, thinkability and occupancy are co-constitutional: expression became ownable because the statutory form made it so, and the form's legitimacy depends on the conceptual stability of the category. From the contesting philosophers' seat, the two might be independent: a prior common-law holder might have benefited from expression being thinkable as property without statutory occupancy changing; a proprietor might hold statutory occupancy while expression remains unthinkably tied to authorship-as-craft rather than authorship-as-property. The customary practitioners sit in a third position: they lost occupancy but also had to accept the new category's frame, even though their own prior claim to legitimacy rested on the old thinkability boundaries. The engine computes these divergences from the power/exit/beneficiary structure; the authored metrics show the empirical trajectory but leave the independence test unresolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietors sit near beneficiary end (d ≈ 0.1–0.2): they gain stable occupancy and licensing rents. Prior common-law users sit near target end (d ≈ 0.8–0.9): they lose occupancy and must either license or cease; their exit is identity-locked because their prior claim to legitimacy rested on texts being reproducible. Customary practitioners are trapped (d ≈ 0.85–0.95): their craft becomes illegitimate under the new category, and their alternatives (licensing, other trades) are all constrained by the category's enforcement. Doctrine builders are near symmetric (d ≈ 0.5): they construct and defend the category, but their authority depends on it remaining coherent, so they bear a stability cost alongside their institutional gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified reproduction regime vs. guild/crown privilege fragmentation) is live in 1710 but contested by 1800. The statute solved occupancy unification (proprietors now hold stable rights); whether it solved thinkability coherence depends on whether thinkability was the real problem or a cover story. If the real problem was occupancy fragmentation and thinkability coherence was auxiliary, the constraint is a tangled rope with a weakening coordination function — the theater ratio's rise (0.05 → 0.38) supports this reading. If thinkability coherence was the fundamental problem, the constraint remains a genuine rope solving a real coordination challenge. The M4/M5 collapse test (via omega variable) is precisely the device for resolving this: does empirical evidence show thinkability and occupancy varying independently? If yes, the constraint is authentically rope-like (two independent problems, both solved). If no, the constraint is increasingly a snare (occupancy extraction disguised as category coherence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_occupancy_independence,
    'Are thinkability (expression''s conceptual coherence as ownable property) and first-holding (the transfer of occupancy from custom to statute) formally independent, or do they always co-occur and therefore constitute a single kernel claim with a misleading dual description?',
    'Historical decomposition: find evidence of (1) expression becoming thinkable as property BEFORE statutory occupancy transferred (supporting independence), OR (2) occupancy transferring WITHOUT expression becoming newly thinkable (same support), OR (3) alternative jurisdictions where one occurs without the other. Alternatively, theoretical analysis: construct a logical model where the two vary independently and ask whether any such model fits the pre-1710 or post-1710 empirical record.',
    'If independent: both thinkability_reading and first_holding_reading capture distinct kernel constraints; the synchronic_diachronic_seam reading is a valid diagnostic. If co-occurring: the two readings are observationally equivalent and the distinction is a temporal framing artifact; the kernel is simpler than the dual-reading framework assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thinkability_occupancy_independence, empirical, 'Whether category emergence (M4) and occupancy change (M5) are independent or spurious dual descriptions of a single event.').

omega_variable(
    concept_prior_vs_statute_driven,
    'Did expression become thinkable as property because the statutory form emerged (statute_driven), or did the statute succeed because expression had already become thinkable in prior doctrine and practice (concept_prior)?',
    'Close reading of pre-1710 legal writing (Common Pleas reports, equity doctrine, guild records, privilege grants) to detect whether expression-as-property framing appears before the statute. If present, the statute codified a prior conceptual shift; if absent, the statute created the concept.',
    'Concept_prior supports independence: thinkability existed before occupancy shifted. Statute_driven suggests co-constitution: the statute''s form made thinkability coherent for the first time. The resolution determines whether doctrine builders in 1710 were responding to or generating the concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concept_prior_vs_statute_driven, empirical, 'Whether thinkability precedes or follows statutory occupancy transfer.').

omega_variable(
    doctrine_builder_intentionality,
    'When legal doctrine builders (judges, parliamentarians, jurists) defended or expanded copyright in the 1710–1800 period, were they consciously maintaining a thinkability-occupancy distinction, or was the distinction implicit in their reasoning and later reconstructed by historians?',
    'Archival analysis: read judicial opinions, parliamentary debates, and jurisprudential commentary for explicit reference to thinkability vs. occupancy as separate concerns. If the distinction appears in period sources, it was live; if it appears only in modern scholarship, it is a retrospective interpretive frame.',
    'If explicit in period sources: the doctrine builders were aware of and held the distinction; the reading''s structural autonomy is historically grounded. If retrospective: the distinction is an analytical imposition and may not track real doctrinal choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_builder_intentionality, empirical, 'Whether the synchronic-diachronic seam was present in period doctrine or is a modern analytical frame.').

omega_variable(
    suppression_necessity_question,
    'Is the rising suppression requirement (0.08 → 0.41) intrinsic to maintaining thinkability (the category must be actively defended as coherent), or intrinsic to maintaining occupancy (customary practitioners must be prevented from competing), or both equally?',
    'Counterfactual analysis: if customary practitioners were permitted to continue their work under an explicit license (occupancy relaxed), would the category''s thinkability remain stable or would it degrade? If it remains stable, suppression was occupancy-driven; if it degrades, suppression is thinkability-necessary. Apply across jurisdictions with different suppression levels.',
    'Occupancy-driven suppression suggests the constraint is snare-like (extraction masked as coherence). Thinkability-driven suppression suggests the constraint is genuinely rope-like (coherence requires active maintenance). Mixed answers support the independence hypothesis: one suppression mechanism for each.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_necessity_question, empirical, 'Whether suppression maintains conceptual coherence or occupancy exclusivity (or both inseparably).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1650, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1650, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1650, 0.05).
narrative_ontology:measurement(ip_c_tr_t1680, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1680, 0.12).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.28).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1740, 0.35).
narrative_ontology:measurement(ip_c_tr_t1770, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1770, 0.38).
narrative_ontology:measurement(ip_c_tr_t1800, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1800, 0.38).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1650, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1650, 0.15).
narrative_ontology:measurement(ip_c_be_t1680, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1680, 0.28).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.52).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1740, 0.58).
narrative_ontology:measurement(ip_c_be_t1770, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1770, 0.61).
narrative_ontology:measurement(ip_c_be_t1800, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1800, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1650, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1650, 0.08).
narrative_ontology:measurement(ip_c_su_t1680, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1680, 0.18).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1710, 0.35).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1740, 0.39).
narrative_ontology:measurement(ip_c_su_t1770, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1770, 0.41).
narrative_ontology:measurement(ip_c_su_t1800, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1800, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__synchronic_diachronic_seam, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The synchronic_diachronic_seam reading is part of the ip_category_emergence kernel family. The kernel contest holds three readings: thinkability_reading (category emergence as conceptual breakthrough), first_holding_reading (category emergence as occupancy transfer), and this reading (the seam testing whether they are formally independent or spurious dual descriptions). All three share the same referent (the Statute of Anne and its consequences) but instantiate different constraint structures depending on how the change is characterized. The seam reading's ε measures the composite constraint that BOTH changes occurred and are inseparable; sibling readings measure constraints where one change is primary. This reading links to both siblings via affects_constraints; each sibling should reciprocally link to this one and to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
