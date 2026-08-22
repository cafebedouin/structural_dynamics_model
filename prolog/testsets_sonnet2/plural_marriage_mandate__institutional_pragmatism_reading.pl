% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Strategic Institutional Adaptation (Doctrine-as-Legitimation Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This story instantiates the institutional-pragmatism reading of the 1890
 *   Manifesto kernel: the doctrinal revelation narrative surrounding the
 *   Manifesto is treated as a legitimation device layered over a
 *   survival-driven accommodation to overwhelming federal coercive power. On
 *   this reading the primary observable is the gap between the public
 *   declaration (practice ended) and the documented private record (practice
 *   continued under hierarchical authorization through roughly 1904), which
 *   the institutional-pragmatism frame treats as diagnostic rather than
 *   incidental. This is one of three readings of the same kernel; the
 *   endogenous_reinterpretation_reading treats the same document as genuine
 *   prophetic revelation, and the exogenous_override_reading treats it as
 *   pure coercive imposition with no legitimating doctrinal content at all.
 *   Each reading is authored as its own constraint with its own epsilon; this
 *   file does not average across them.
 *
 * KEY AGENTS:
 *   - church_hierarchy_leadership: institutional agenda-setter and primary beneficiary, drafts and administers the Manifesto while authorizing concealed continuations
 *   - coerced_polygamist_families and post_manifesto_plural_wives: bear direct legal and social extraction from a policy publicly disavowed but privately continued
 *   - deceived_monogamist_converts: bear extraction through institutional misrepresentation of the practice's actual status
 *   - federal_government: the coercive power the Manifesto is nominally responding to, itself only partially informed of the actual accommodation
 *   - historians_and_dissenting_scholars: analytical observers who establish the M-set gap from the post-1890 marriage record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Strategic Institutional Adaptation (Doctrine-as-Legitimation Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '383a409f-1e6a-4de4-a0eb-d05f6a9dc52c').
narrative_ontology:cs_kernel_codification('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', formalized).
narrative_ontology:cs_authority_grounding('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', extraction).
narrative_ontology:cs_interpretation_layer_present('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c').
narrative_ontology:cs_reading_relation('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', foundational, revelation_claims_are_instrumentalized_legitimation).
narrative_ontology:cs_axiom_status(revelation_claims_are_instrumentalized_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', revelation_claims_are_instrumentalized_legitimation, empirically_contingent).
narrative_ontology:cs_axiom('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', foundational, institutional_survival_is_the_operative_cause_not_doctrine).
narrative_ontology:cs_axiom_status(institutional_survival_is_the_operative_cause_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', institutional_survival_is_the_operative_cause_not_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', revelatory_authority_as_institutional_instrument).
narrative_ontology:cs_drift_state('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', post_reed_smoot_hearings, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('383a409f-1e6a-4de4-a0eb-d05f6a9dc52c', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_corporate_entity).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_converts).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_plural_wives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Manifesto declaring intent to comply with federal anti-polygamy law, publicly frames it as inspired counsel, and simultaneously authorizes and conceals continued plural marriages performed by apostles and other officials through at least 1904. Recovers church property seized under the Edmunds-Tucker Act, regains political viability including eventual statehood, and preserves institutional continuity by converting an existential coercion crisis into a controlled doctrinal narrative it authors and administers.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, beneficiary).

% Existing plural families face disintegration, criminal prosecution risk, and social stigma as the institution that mandated their marriages publicly disavows the practice to satisfy federal pressure, leaving them to absorb the legal and social fallout of a commitment the hierarchy no longer defends in the open while quietly continuing to authorize it for insiders.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Enter plural marriages performed secretly after 1890 under continued authorization from leadership, believing the practice remains divinely sanctioned even as the institution publicly denies its existence; they carry all legal exposure and social risk of a practice the hierarchy has made deniable at the top while sustaining it underneath.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_plural_wives, payer,
    powerless, biographical, trapped, regional).

% Join or remain in the church on the understanding, reinforced by repeated public denials before congressional committees and the press, that plural marriage has genuinely ended; they contribute tithing, loyalty, and reputational capital to an institution that is materially misrepresenting its own ongoing practice to them and to civil authorities.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_converts, payer,
    powerless, biographical, constrained, national).

% Applies escalating coercive pressure (Edmunds-Tucker Act, disincorporation, disenfranchisement, property seizure) to compel abandonment of polygamy and accepts the Manifesto as sufficient compliance to restore Utah's path to statehood, without full knowledge of the secret continuations documented in the post-1890 marriage record, meaning the party the institution is nominally capitulating to is not fully informed of the actual accommodation reached.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Ordinary members outside the secret-continuation circle receive the benefit of restored political normalcy and reduced federal antagonism, but bear the cost of institutional dishonesty toward them and toward civil authorities being conducted in their name, and later must reconcile a doctrinal history in which the public narrative diverges from the documented private practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership, beneficiary).

% Examine the post-1890 marriage records, court testimony from the Reed Smoot hearings, and internal correspondence documenting continued plural marriages performed with hierarchical authorization; their scholarship establishes the M-set gap between declared doctrine and actual practice that the institutional-pragmatism reading treats as the central observable.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, historians_and_dissenting_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The institution needed a mechanism to survive an existential coercive campaign (federal disincorporation, property seizure, mass disenfranchisement, imprisonment of leadership) without either total capitulation that would dissolve its cohesion or total resistance that would destroy it outright; the Manifesto coordinates a controlled, staged retreat that preserves institutional continuity, corporate assets, and leadership authority.
% TRANSFER_FUNCTION: Moves legal and social risk from the institution and its leadership onto individual plural families and unknowing converts: the hierarchy transfers the cost of federal compliance theater downward while retaining the benefit of both restored political standing and continued (concealed) practice among its inner circle, and it extracts ongoing loyalty and resources from members who are not told the practice continues.
% ABSENT_VOICES: Post-1890 plural wives and their children, whose marriages the institution's own later leadership would eventually disavow as unauthorized once inconvenient, had no voice in the drafting of a document presented as ending a practice they were simultaneously being authorized to enter; federal negotiators pressing for genuine cessation likewise never had visibility into the secret continuation record.
% DISAPPEARANCE_RATIONALE: Had the Manifesto (and the accommodation it represents) not occurred, the institution faced continued disincorporation, asset forfeiture, and territorial disenfranchisement with no clear resolution path; conversely, had it been a genuine full cessation rather than a managed one, the documented 1890-1904 plural marriages could not have occurred and the 1904 Second Manifesto and subsequent excommunications of continuing practitioners would have no object. The gap between the public document and the private record is itself evidence the arrangement is doing structural work, not merely reporting a settled fact.
% FOUNDING_PROBLEM: Federal anti-polygamy enforcement had escalated to the point of threatening the church's corporate existence, its temples, and its leadership's physical liberty; some resolution — public or otherwise — was required for the institution to continue operating as a legal entity in the United States.
% FOUNDING_PROBLEM_CORROBORATION: Congressional testimony from the 1904-1907 Reed Smoot hearings, external federal investigators, and independent historians examining the post-1890 marriage sealing records corroborate that the founding problem (federal coercive pressure) was real and that the institution's response included both genuine public retreat and concealed continuation; the institution's own later leadership (in issuing the 1904 Second Manifesto) implicitly corroborates that the first Manifesto had not ended the practice, since a second, stricter declaration with enforcement teeth would otherwise have been unnecessary.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial 0.68 because the arrangement transfers concrete legal, financial, and reputational risk onto powerless parties (plural families, deceived converts) while the hierarchy captures the benefits of restored political standing and preserved corporate continuity. Extraction rises through the 1890s as the M-set gap widens (more documented secret marriages, more converts unknowingly relying on the false public narrative) and moderates slightly after 1904 as the Second Manifesto imposes real enforcement. Suppression (0.72) reflects the institution's active management of information — controlling which members know about continuations, disciplining dissenters, and shaping the public congressional narrative — this is a raw structural property, not scaled by scope. Theater ratio is substantial and rising (0.4 to a peak of 0.65 around 1902) because an increasing share of the institution's public compliance activity is performative: testimony to Congress and public statements diverge from the documented internal authorization pattern, which is the theatrical signature that motivates the tangled_rope classification over a clean rope reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy leadership sits at the beneficiary end: institutional power, arbitrage-grade exit (able to renegotiate the terms of compliance unilaterally and administer the gap between declaration and practice), and the entity that recovers seized assets and political standing. Coerced polygamist families and post-manifesto plural wives sit at the target end: powerless, trapped exit, bearing legal and social costs the leadership does not share. Deceived monogamist converts are also targets, though their extraction is less visible — they are targets of misrepresentation rather than direct legal jeopardy, which is why they are listed as payers despite moderate structural power as a body. Rank-and-file membership occupies a genuinely mixed position: real coordination benefit (political normalcy, reduced federal antagonism) alongside real cost (complicity in and later reckoning with institutional dishonesty) — this dual role is why a secondary_role is authored for that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors symmetric to the endogenous and exogenous readings: it does not treat the Manifesto as pure coordination (which would erase the documented victim set of coerced and deceived parties) and it does not treat it as pure extraction with no coordination function (which would erase the genuine institutional-survival problem the leadership faced under existential federal pressure). The founding problem was real and the disappearance_verdict of world_rearranges reflects that the institution's actual survival depended on some resolution; what the institutional-pragmatism reading adds is that the SPECIFIC resolution chosen routed the costs of that survival onto powerless parties while using doctrinal legitimation language to obscure that routing from both federal authorities and the general membership.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_sincerity_vs_instrumentalization,
    'Did church leadership in 1890 sincerely believe they had received genuine revelation ending the mandate, or did they consciously deploy revelatory language as legitimating cover for a decision reached primarily on institutional-survival grounds?',
    'This is likely irreducible: private diaries and correspondence from Wilford Woodruff and other leaders show mixed language consistent with both sincere belief and strategic framing; the documented authorization of continued secret marriages through 1904 is consistent with either sincere-but-narrow revelation (ending only the PUBLIC mandate) or fully instrumental use of the revelation narrative. No single document resolves this cleanly, which is why three separate readings are authored as separate constraints rather than reconciled into one.',
    'If sincere, this reading''s extractiveness attribution to leadership intent overstates the case and the endogenous_reinterpretation_reading is the more accurate structural account; if instrumental, this reading''s tangled_rope classification with leadership as knowing beneficiary is the accurate account. The M-set gap (documented secret continuations) is the fact most readings must accommodate regardless of which resolution is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_sincerity_vs_instrumentalization, conceptual, 'Whether the 1890 revelation narrative reflects sincere belief or conscious instrumentalization — irreducible to available documentary evidence.').

omega_variable(
    scope_of_leadership_knowledge_of_continuations,
    'How many members of the church hierarchy knew of and authorized the post-1890 secret plural marriages, versus how many genuinely believed the practice had ended, and did this knowledge shift over the 1890-1904 period?',
    'Cross-reference of authorization records (who performed or approved the secret marriages) against public statements and private correspondence of the full quorum membership across the period; the 1904 Reed Smoot hearings produced sworn testimony that partially but not completely resolves this.',
    'A narrow authorizing circle (a few apostles) versus broad hierarchical knowledge changes whether ''church_hierarchy_leadership'' as authored here is accurately scoped as a single beneficiary seat or should be split into a knowing-inner-circle beneficiary seat and a genuinely-deceived-leadership payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_leadership_knowledge_of_continuations, empirical, 'The scope and distribution of knowledge about secret continuations within the leadership body itself.').

omega_variable(
    federal_government_partial_knowledge,
    'Did federal officials who accepted the Manifesto as sufficient for statehood negotiations have partial knowledge of ongoing secret continuations and accept the arrangement as a face-saving fiction, or were they genuinely deceived?',
    'Examination of federal investigator reports and territorial governor correspondence from 1890-1896 for evidence of awareness or willful non-investigation.',
    'If federal officials tacitly accepted a face-saving fiction, the coercive relationship itself becomes a negotiated tangled_rope between two institutional actors rather than a one-sided deception of the federal_government seat as currently authored (excluded, low informational access); this would justify reclassifying federal_government toward an agenda_setter or co-beneficiary role in a revised structural account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_government_partial_knowledge, empirical, 'Whether federal acceptance of the Manifesto involved tacit knowledge of continued practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.65).
narrative_ontology:measurement(plur_tr_t1906, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1906, 0.6).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.58).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.62).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.66).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.7).
narrative_ontology:measurement(plur_be_t1906, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1906, 0.72).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.68).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.7).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.78).
narrative_ontology:measurement(plur_su_t1906, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1906, 0.75).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, second_manifesto_1904_enforcement_mandate).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the plural_marriage_mandate kernel (the 1890 Manifesto), each authored as a separate ε-invariant constraint per the ε-invariance principle: endogenous_reinterpretation_reading (Mountain/Rope-leaning: genuine prophetic reinterpretation, low extraction, coordination-dominant), exogenous_override_reading (extraction attributed to the external coercive party, institution as target rather than agent), and this institutional_pragmatism_reading (tangled_rope: doctrinal legitimation entangled with survival-driven extraction routed onto powerless parties). The three share the same historical document and the same interval but diverge sharply on beneficiary/victim structure and on where agency and extraction are located. They are linked via affects_constraints rather than merged because averaging their ε values would misrepresent all three positions as a single incoherent hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
