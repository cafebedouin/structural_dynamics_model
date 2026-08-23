% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Institutional Survival Mechanism
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) suspended the practice of
 *   plural marriage in the LDS Church under pressure from federal
 *   anti-polygamy legislation that had disincorporated the church and seized
 *   its assets. This reading frames the Manifesto as a strategic
 *   institutional adaptation: leadership capitulated to superior coercive
 *   power while deploying a revelation narrative to legitimate the reversal.
 *   The constraint is the resulting arrangement — doctrine unchanged,
 *   practice publicly suspended, secret continuations authorized (1890-1904),
 *   with leadership as primary beneficiary (institutional survival, restored
 *   rights) and both polygamists and monogamists as victims (coerced
 *   abandonment, doctrinal deception). The claimed type is tangled_rope:
 *   genuine coordination function (institutional survival) entangled with
 *   asymmetric extraction (sacrificing committed members for institutional
 *   continuity), requiring active enforcement (purging dissenters, monitoring
 *   compliance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Institutional Survival Mechanism").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '7a1fc5e2-04d2-4ee9-931d-8ebd57597b50').
narrative_ontology:cs_kernel_codification('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', formalized).
narrative_ontology:cs_authority_grounding('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', lineage).
narrative_ontology:cs_interpretation_layer_present('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50').
narrative_ontology:cs_reading_relation('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', foundational, institutional_survival_justifies_doctrinal_adaptation).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_doctrinal_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', institutional_survival_justifies_doctrinal_adaptation, instrumental).
narrative_ontology:cs_axiom('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', foundational, revelation_narrative_as_legitimation_instrument).
narrative_ontology:cs_axiom_status(revelation_narrative_as_legitimation_instrument, holdable).
narrative_ontology:cs_axiom_grounding('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', revelation_narrative_as_legitimation_instrument, instrumental).
narrative_ontology:cs_reference_frame('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', prophetic_authority_framework).
narrative_ontology:cs_drift_state('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', post_manifesto_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a1fc5e2-04d2-4ee9-931d-8ebd57597b50', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, monogamist_congregants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, monogamist_congregants).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_justifies_doctrinal_adaptation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto suspending plural marriage practice while maintaining the doctrine. Secured Utah statehood, returned confiscated assets, and regained political legitimacy. Publicly framed the suspension as divine revelation; privately authorized continued plural marriages (1890-1904). Collected institutional survival and restored institutional prerogatives as the primary beneficiary.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Had built families, economic structures, and spiritual identities around plural marriage as a divine commandment. The Manifesto demanded they abandon wives and children or face excommunication, while leadership quietly permitted new plural marriages. Their exit was blocked by identity fusion: plural marriage constituted their salvation narrative, community standing, and family integrity. Many went into hiding, fled to Mexico/Canada, or lived in legal limbo.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, practicing_polygamists, payer,
    organized, biographical, identity_locked, national).

% Were taught plural marriage was essential for exaltation, then told it was suspended by revelation. Those who had sacrificed for the principle felt deceived; those who never practiced gained institutional stability and social acceptance. Their exit options were constrained by community ties, temple access, and the doctrinal claim that the Manifesto came from God. They bore the cognitive cost of doctrinal whiplash while gaining mainstream legitimacy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, monogamist_congregants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, monogamist_congregants, beneficiary).

% Apostles and leaders (e.g., Matthias Cowley, John W. Taylor) who refused to accept the Manifesto as binding. They continued performing plural marriages and were eventually excommunicated or silenced. Their structural exclusion was the enforcement mechanism: the Manifesto's legitimacy required purging internal dissent. They had no exit that preserved their apostolic authority within the institution.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, dissenting_fundamentalists, excluded,
    powerful, biographical, trapped, national).

% Applied coercive pressure (Edmunds Act, Edmunds-Tucker Act, disfranchisement, asset seizure) that made the Manifesto a survival necessity. After 1890, they monitored compliance through the Smoot hearings and Reed Smoot's Senate seating battle. Their interest was political: ending theocratic governance in Utah, not theological correctness. They function as the external coercive referent the constraint adapts to.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Analyze the Manifesto as a case study in religious accommodation to state power. Access archives, diaries, and minutes that reveal the gap between public revelation narrative and private authorization of continued plural marriages. Their analytical seat sees the full structure: the coordination function (institutional survival), the extraction (polygamists' sacrifice, congregants' trust), and the legitimation machinery (revelation narrative).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, academic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secured the institutional survival of the LDS Church by negotiating a viable political settlement with the United States federal government, averting disincorporation and asset seizure while preserving the church's corporate existence and leadership structure.
% TRANSFER_FUNCTION: Moves institutional survival and restored political rights from federal concession to church leadership; moves doctrinal coherence, family integrity, and spiritual trust from practicing polygamists and monogamist congregants to leadership; moves the cost of federal coercion from the institution onto its most committed members.
% ABSENT_VOICES: Dissenting apostles (Cowley, Taylor) and fundamentalist factions who would have objected to capitulation were structurally excluded through excommunication and silencing; their objection would have fractured the revelation narrative's legitimacy. Polygamist women's voices were largely absent from the decision calculus — the Manifesto was negotiated by male leadership without consulting the women whose marriages and children were most directly affected.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished overnight, the church would lose its legal standing (reverting to pre-1890 disincorporated status), federal prosecution of polygamists would resume, Utah statehood would be jeopardized, and the leadership would face imprisonment or exile. The polygamist communities would re-emerge openly. The entire political settlement of 1890-1896 would collapse.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation (Edmunds Act 1882, Edmunds-Tucker Act 1887) had disincorporated the church, seized its assets, disfranchised its members, and threatened leadership with imprisonment — creating an existential threat to the institution's corporate existence.
% FOUNDING_PROBLEM_CORROBORATION: Non-LDS historians (Sarah Barringer Gordon, Kathleen Flake) and the congressional record confirm the legal threat was resolved by the Manifesto and subsequent statehood; the Edmunds-Tucker Act's penalties were lifted after 1890. The church's own institutional archives document the leadership's strategic calculation. The founding problem (existential legal threat) is objectively dead, though the arrangement it produced persists.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high (0.72 peak) because leadership extracted survival at the cost of polygamists' families and congregants' trust. Suppression is very high initially (0.85) — federal coercion created the constraint, and internal enforcement (excommunications, loyalty tests) maintained it. Theater ratio peaks at 0.52 during 1900-1904 (secret continuations while publicly denying them) then declines after the 1904 Second Manifesto made enforcement genuine. The measurement grid uses one shared time axis (years since 1890) so all metrics are authored at each point — the engine samples the union of time points.
 *
 * PERSPECTIVAL GAP:
 *   From leadership's seat, the Manifesto is a rope: genuine coordination solving an existential collective-action problem (federal destruction). From polygamists' seat, it is a snare: the revelation narrative is cover for capitulation; their sacrifice buys leadership's survival. From monogamists' seat, it is a tangled rope: they gain mainstream acceptance but at the cost of doctrinal whiplash. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the analytical seat's assessment of the constraint's aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is the structural beneficiary (d near 0.0): collects institutional survival, political rights, asset return; controls the narrative; has arbitrage-grade exit (could have fled or fought). Practicing polygamists are full targets (d near 1.0): identity-locked exit makes the constraint inescapable without spiritual self-annihilation; they bear the extraction. Monogamist congregants sit near symmetric (d ~0.5): gain institutional stability but lose doctrinal coherence. Dissenting fundamentalists are excluded — their exclusion IS the enforcement. Federal authorities are the external coercive referent, not a seat inside the constraint. Academic historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential legal threat) is dead — resolved by statehood and changed political conditions. Yet the constraint persists: the revelation narrative remains binding, plural marriage remains doctrinally required but practically forbidden, and the institutional adaptation has become the church's settled self-understanding. This is mandatrophy: the survival mechanism outlived its founding necessity and became a permanent doctrinal fixture. The classification prevents mislabeling this as pure coordination (rope) because the extraction from polygamists was asymmetric and enforced; it prevents mislabeling as pure extraction (snare) because the coordination function (institutional survival) was real and benefited the whole community. Tangled rope captures the entanglement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_narrative_authenticity,
    'Is the Manifesto''s revelation narrative a genuine prophetic experience or a strategic fabrication? The classification hinges on this: genuine revelation supports endogenous_reinterpretation_reading (rope/mountain); instrumental narrative supports institutional_pragmatism_reading (tangled_rope/snare).',
    'No empirical resolution possible — the claim is epistemically private to Woodruff and the Quorum. Assessment depends on whether one treats prophetic authority as self-authenticating (internal to the tradition) or as a claim requiring external validation. The engine cannot adjudicate; this omega records the framing under-determination.',
    'If genuine, the constraint coordinates belief and practice through divine authority (lower extraction). If instrumental, the constraint extracts trust from members to serve institutional survival (higher extraction). The difference shifts effective extraction by ~0.2-0.3 for identity-locked agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_narrative_authenticity, conceptual, 'Whether the revelation claim is epistemically genuine or strategically deployed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by polygamists structural (legal penalties, excommunication) or internalized (identity fusion making exit unthinkable)? The measured suppression (0.78) conflates both.',
    'Post-exit suppression trajectory: track polygamists who left for Mexico/Canada or joined fundamentalist groups. If suppression persists after geographic/institutional exit (shame, family rupture, spiritual crisis), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This raises effective extraction for identity-locked agents beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked polygamists.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (institutional_pragmatism_reading) of the contested kernel plural_marriage_mandate. How does the reading relation structure affect classification?',
    'The engine computes per-reading classifications independently. The kernel context is recorded here and in cs_structure.reading_relations/cs_structure.axioms. Cross-reading comparison is a meta-analytic task, not an engine function.',
    'The sibling readings produce different ε values and different beneficiary/victim structures. Exogenous override reads higher extraction (federal coercion as pure extraction). Endogenous reinterpretation reads lower extraction (divine coordination). This reading reads substantial extraction with genuine coordination — the tangled_rope signature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel.').

omega_variable(
    secret_continuations_as_separate_constraint,
    'Do the authorized post-Manifesto plural marriages (1890-1904) constitute a separate constraint (a snare operating under cover of the Manifesto''s rope), or are they part of the same tangled_rope?',
    'Decompose per ε-invariance: if the secret continuations have different beneficiaries (mid-level leadership vs. top leadership), different victims (new plural wives vs. existing families), and different enforcement (secrecy vs. public compliance), they are a separate constraint. Link via network.affects_constraints.',
    'If separate, the Manifesto constraint is a scaffold (temporary cover) and the secret continuations are a snare. If unified, the tangled_rope classification holds with higher theater_ratio during 1890-1904.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secret_continuations_as_separate_constraint, conceptual, 'Whether post-Manifesto secret plural marriages are a distinct constraint or part of this one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t0, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t5, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t10, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t14, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 14, 0.52).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t20, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_tr_t30, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t0, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t5, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t10, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t14, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 14, 0.72).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t20, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_be_t30, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t0, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t5, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t10, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t14, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t20, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(plural_marriage_mandate__institutional_pragmatism_reading_su_t30, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__institutional_pragmatism_reading, 0.08).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel plural_marriage_mandate by treating the revelation narrative as instrumental legitimation for institutional survival. The exogenous_override_reading treats federal coercion as the sole driver (higher ε, snare-type). The endogenous_reinterpretation_reading treats the revelation as genuine (lower ε, rope/mountain-type). The three readings share the same referent (the 1890 Manifesto and its aftermath) but author different ε, different beneficiary/victim structures, and different constraint types — per the ε-invariance principle, they are distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, institutional, 0.15).
constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
