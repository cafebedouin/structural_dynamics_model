% ============================================================================
% CONSTRAINT STORY: substantive_due_process__privacy_line_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substantive_due_process_privacy_line_reading, []).

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
 *   constraint_id: substantive_due_process__privacy_line_reading
 *   human_readable: Substantive Due Process: Privacy Line Reading (Intimate Governance Doctrine)
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   The privacy line reading of substantive due process establishes that
 *   intimate governance (contraception, marriage, child-rearing, sexual
 *   orientation) is constitutionally protected from majoritarian morals
 *   regulation. The state cannot enter the private sphere without compelling
 *   interest and narrow tailoring. This reading instantiates one
 *   interpretation of the Fourteenth Amendment's Due Process Clause —
 *   distinct from the history-tradition test reading (Glucksberg: only rights
 *   deeply rooted in history and tradition) and from the economic liberty
 *   reading (Lochner era). The privacy reading's structural effect is to
 *   create an asymmetry: households gain autonomous decision-making space;
 *   majoritarian morals coalitions lose access to a regulatory domain; the
 *   doctrine's protective force depends on judicial will and faces reversal
 *   risk. The constraint exhibits the full range of DR types from different
 *   positions: households without the doctrine face snare dynamics (trapped,
 *   suppressed, no legal exit); households with the doctrine benefit from
 *   mixed coordination and autonomy (tangled rope); privacy-rights
 *   organizations experience rope coordination (stable institutional
 *   position); majoritarian coalitions experience snare (extraction of their
 *   regulatory authority); judges experience scaffold (temporary power with
 *   uncertain duration); the civilizational analytical view risks
 *   naturalizing the doctrine as foundational law (false summit).
 *
 * KEY AGENTS:
 *   - Households seeking intimate autonomy (moderate/constrained): Primary beneficiary of doctrinal protection; experience tangled rope (both autonomy gain and litigation burden)
 *   - Majoritarian morals coalitions and state legislatures (organized/constrained): Primary victim set whose regulatory authority is fenced out by the privacy doctrine; experience snare (suppression of moral legislation without exit path)
 *   - Privacy-rights advocacy organizations (institutional/arbitrage): Secondary beneficiary; experience rope (doctrine enables their advocacy work and resource allocation)
 *   - Supreme Court (powerful/arbitrage): Judicial gatekeeper; experiences scaffold dynamics (doctrine persists only at judicial will)
 *   - Households without doctrinal protection (powerless/trapped): Pre-doctrine or post-reversal position; experience snare (maximal suppression without constitutional exit)
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent doctrine as foundational right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substantive_due_process__privacy_line_reading, 0.32).
domain_priors:suppression_score(substantive_due_process__privacy_line_reading, 0.48).
domain_priors:theater_ratio(substantive_due_process__privacy_line_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substantive_due_process__privacy_line_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(substantive_due_process__privacy_line_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substantive_due_process__privacy_line_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substantive_due_process__privacy_line_reading, tangled_rope).
narrative_ontology:human_readable(substantive_due_process__privacy_line_reading, "Substantive Due Process: Privacy Line Reading (Intimate Governance Doctrine)").
narrative_ontology:topic_domain(substantive_due_process__privacy_line_reading, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(substantive_due_process__privacy_line_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substantive_due_process__privacy_line_reading, '1ae03fb2-e1e6-44e5-855e-fb8328d19659').
narrative_ontology:cs_kernel_codification('1ae03fb2-e1e6-44e5-855e-fb8328d19659', fixed_text).
narrative_ontology:cs_authority_grounding('1ae03fb2-e1e6-44e5-855e-fb8328d19659', lineage).
narrative_ontology:cs_interpretation_layer_present('1ae03fb2-e1e6-44e5-855e-fb8328d19659').
narrative_ontology:cs_reading_relation('1ae03fb2-e1e6-44e5-855e-fb8328d19659', substantive_due_process__history_tradition_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ae03fb2-e1e6-44e5-855e-fb8328d19659', substantive_due_process__lochner_economic_liberty_reading, influences).
narrative_ontology:cs_axiom('1ae03fb2-e1e6-44e5-855e-fb8328d19659', foundational, intimate_autonomy_protected_from_moral_regulation).
narrative_ontology:cs_axiom_status(intimate_autonomy_protected_from_moral_regulation, holdable).
narrative_ontology:cs_axiom_grounding('1ae03fb2-e1e6-44e5-855e-fb8328d19659', intimate_autonomy_protected_from_moral_regulation, deontological).
narrative_ontology:cs_axiom('1ae03fb2-e1e6-44e5-855e-fb8328d19659', foundational, privacy_boundary_fences_out_state_entry).
narrative_ontology:cs_axiom_status(privacy_boundary_fences_out_state_entry, holdable).
narrative_ontology:cs_axiom_grounding('1ae03fb2-e1e6-44e5-855e-fb8328d19659', privacy_boundary_fences_out_state_entry, deontological).
narrative_ontology:cs_reference_frame('1ae03fb2-e1e6-44e5-855e-fb8328d19659', household_decisional_autonomy_from_state_morals_enforcement).
narrative_ontology:cs_drift_state('1ae03fb2-e1e6-44e5-855e-fb8328d19659', contemporary_post_dobbs, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1ae03fb2-e1e6-44e5-855e-fb8328d19659', '').
narrative_ontology:cs_kernel_id(substantive_due_process__privacy_line_reading, substantive_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substantive_due_process__privacy_line_reading, household_decisional_autonomy).
narrative_ontology:constraint_victim(substantive_due_process__privacy_line_reading, majoritarian_morals_legislation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD WITHOUT PROTECTION (SNARE) — Prior to this reading's doctrinal establishment, households subject to majoritarian morals legislation face suppression without exit. The state criminalizes contraception, sodomy, or marriage choices; households cannot legally challenge or exit. Maximum experienced extraction — suppression of intimate choice for those lacking constitutional shelter.
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOUSEHOLD WITHIN DOCTRINE (TANGLED ROPE) — Once the privacy reading is established, households gain a coordination function: the doctrine enables intimate decision-making without state interference. But enforcement is costly and uncertain — litigation required, risk of doctrinal reversal, uneven application across judges and jurisdictions. Household experiences both genuine autonomy gain and extraction through litigation burden and doctrinal vulnerability.
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVACY-RIGHTS COALITION (ROPE) — Institutional actors (ACLU, reproductive-rights organizations, LGBTQ+ legal advocates) benefit from the privacy reading as a coordination mechanism: it establishes precedent that enables their advocacy and resource allocation. The doctrine creates a stable framework within which these organizations operate — low effective extraction because they have strong agency and favorable structural positioning.
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJORITARIAN MORALS COALITIONS (SNARE) — State legislatures and moral-conservative coalitions experience the privacy reading as extraction: their ability to enforce community morals through law is fenced out. They cannot access the regulatory domain once the Court declares intimate decisions protected. Exit options are severely constrained — they can only attempt doctrinal reversal (capital-intensive, uncertain) or work at subconstitutional levels (clinic regulations, waiting periods, fetal personhood claims that try to reframe the victim set).
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL GATEKEEPERS (SCAFFOLD) — The Supreme Court exercises enormous power in this structure: they can grant or withdraw the privacy protection at will. The doctrine functions as a temporary ceiling on state power — it persists only while the Court maintains it. Justices experience the constraint as having a clear sunset: when composition shifts, the protection can be narrowed or eliminated. The doctrine's functional life is bounded by institutional will.
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FOUNDATIONAL RIGHTS VIEW (MOUNTAIN) — From a civilizational perspective, intimate autonomy is presented as a foundational right that flows naturally from human dignity and self-determination — not a contingent doctrinal invention but an irreducible feature of free societies. This perspective sees the privacy boundary as immutable law grounded in natural personhood, not as a constructed legal doctrine. However, the structural data reveals this as a false summit: the 'foundational' framing masks the doctrinal construction and the institutional vulnerability of the reading.
constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substantive_due_process__privacy_line_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substantive_due_process__privacy_line_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(substantive_due_process__privacy_line_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The privacy reading fences out state regulation of intimate decisions, which reduces household suppression and provides genuine autonomy gain. However, extractiveness is not negligible because the doctrine's protective force is incomplete: subconstitutional regulations (waiting periods, parental consent, spousal notification) persist; the protection depends on litigation capacity (which households often lack); the doctrine itself has been subject to erosion and reversal (Dobbs); and institutional actors (judges, legislators) retain enormous discretion in applying the doctrine's boundaries. Measured from the household perspective, extractiveness reflects the asymmetry between doctrinal promise and practical enforcement. Suppression (0.48): Moderate-high. For majoritarian morals coalitions and state legislatures, the doctrine suppresses their preferred regulatory outcomes. They cannot directly enact morals legislation in protected domains, though they retain subconstitutional pathways. For households, pre-doctrine suppression was high (~0.85+), post-doctrine suppression dropped sharply. The measured suppression (0.48) reflects the post-doctrine state: households retain some barriers (litigation burden, doctrinal vulnerability, uneven judicial enforcement), while majoritarian coalitions face structural barriers to moral regulation but retain regulatory workarounds. Theater ratio (0.38): Low-moderate. The privacy doctrine's functional content is genuine — households do obtain protection, judges do enforce the doctrine, privacy advocates do gain institutional power — but performative elements exist: the doctrine's boundaries are rhetorically constructed (penumbras, unenumerated rights), judicial justifications shift with composition (Griswold's penumbral reasoning vs. Casey's reliance interest vs. post-Dobbs tiers-of-scrutiny reasoning), and the doctrine's stated rationale often masks institutional interests (legitimacy claims, status quo bias). Theater ratio is not high because the doctrine has material effects — it actually changes regulatory outcomes — but it is elevated above purely functional coordination because the justificatory narratives are contestable.
 *
 * PERSPECTIVAL GAP:
 *   The privacy reading produces a profound perspectival divergence across the constitutional landscape. Households see autonomy and protection (Tangled Rope, with both coordination benefit and litigation burden). Majoritarian coalitions see suppression and lost regulatory power (Snare, with no exit path except doctrinal reversal). Privacy advocates see stable institutional position and resource availability (Rope). Judges see discretionary power with uncertain duration (Scaffold). The household without the doctrine sees maximal extraction and suppression (Snare). The civilizational view risks seeing natural law (Mountain, false summit). The gap reflects that the privacy reading reallocates power: it transfers regulatory authority from democratically elected legislatures to individual households (and to courts as arbiters of the doctrine). This reallocation is genuinely extractive for majoritarian coalitions and genuinely protective for households, which is why the indexical classifications diverge so sharply. The constraint is not naturally law-like — it is a constructed doctrinal boundary that benefits some structural positions and harms others.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values for each perspective are derived from the agent's relationship to the extraction flow and their structural exits. Households within the doctrine experience moderate extraction (d ~0.45) because they benefit from autonomy but bear litigation costs and doctrinal vulnerability. Majoritarian coalitions experience high extraction (d ~0.75) because the doctrine directly suppresses their preferred regulatory outcomes without providing alternative benefit. Privacy-rights organizations experience low extraction (d ~0.20) because the doctrine enables their work and provides institutional power. Judges experience very low extraction (d ~0.05) because they control the doctrine and benefit from its authority. The powerless household without protection experiences maximum extraction (d ~0.95). The analytical observer is positioned as external to the extraction flow (d ~0.72, canonical for analytical perspective). The perspectival gap arises because beneficiaries and victims have opposite directionality: households benefit from the doctrine and experience low experienced extractiveness (χ low); majoritarian coalitions are suppressed and experience high experienced extractiveness (χ high). The doctrine's extractiveness is not uniformly distributed — it is asymmetrically directed toward majoritarian interests and away from household autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The privacy reading resolves mandatrophy by clarifying that extractiveness is not about absolute constraint severity but about the asymmetric allocation of governance authority. The reading does constrain state power (extractiveness ≥ 0.30), but it provides genuine coordination benefits (households can make intimate decisions autonomously; the doctrine enables institutional advocacy). This is the definition of Tangled Rope: both genuine coordination function AND asymmetric extraction present simultaneously. The coordination function is real — the doctrine enables household autonomy and advocacy. The extraction is real — majoritarian coalitions lose regulatory access. No type is 'correct' across all perspectives; the presheaf over the context site is the answer. Households experience the doctrine as Rope or Tangled Rope (depending on their structural position and litigation capacity). Majoritarian coalitions experience it as Snare. Judges experience it as Scaffold. The analytical view that sees it as Mountain (natural law grounded in human dignity) is a false summit — the perspective naturalizes a contingent doctrinal choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_boundary_scope_indeterminacy,
    'What counts as ''intimate governance'' protected from state regulation versus ''conduct regulation'' permissible for public health or child welfare purposes?',
    'Doctrinal test evolution: trace which regulations the Court upholds vs strikes down; identify whether boundary tracks a consistent principle (bodily autonomy, family autonomy, decisional autonomy) or shifts with Court composition',
    'If boundary is principle-based: extractiveness remains ~0.32, victims remain majoritarian morals coalitions. If boundary is compositionally determined: extractiveness rises to ~0.55+ (the doctrine becomes pure power allocation), victims expand to include any household whose intimate choice falls outside Court''s current protection zone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privacy_boundary_scope_indeterminacy, empirical, 'Scope indeterminacy of intimate governance protected sphere').

omega_variable(
    doctrinal_entrenchment_vs_reversibility,
    'Is the privacy line reading anchored deeply enough in precedent and public expectation to resist reversal, or does it remain contingent on Court composition?',
    'Historical analysis of Roe/Casey/Dobbs trajectory; identification of whether privacy doctrine is now understood as bedrock by legal community, legislature, and public or as potentially revisable;public opinion stability around contraception, marriage equality, sodomy decriminalization',
    'If deeply entrenched: doctrine stabilizes, suppression of majoritarian coalitions becomes structural, constraint moves toward piton (institutional inertia). If reversible: doctrine''s protective function is phantom — households'' structural position depends on perpetual judicial support, constraint moves toward scaffold (temporary coordination with unknown sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_entrenchment_vs_reversibility, empirical, 'Whether privacy doctrine is entrenched or compositionally reversible').

omega_variable(
    kernel_contest_logical_structure,
    'What is the logical relationship between the three kernel readings? Does the privacy line reading foreclose the history-tradition test, or do they coexist as live interpretive options?',
    'Doctrinal analysis: can a single legal framework hold both the privacy line reading (fundamental right to intimate autonomy) AND the history-tradition test (only rights deeply rooted in history and tradition qualify)? If both can coexist, they are not foreclosing; if one premise directly contradicts the other, foreclosure relation applies.',
    'If foreclosed: only one reading can be judicially operative at a time. If coexists: both readings are live positions held by different judicial factions, and the constraint''s extractiveness reflects the instability of joint commitment. Determines reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_logical_structure, conceptual, 'Logical structure of kernel contest among three SDP readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of majoritarian morals legislation genuine doctrinal constraint, or does the state retain effective regulatory pathways (parental consent, waiting periods, fetal personhood claims, spousal notification) that convert the doctrine into performative constraint?',
    'Empirical study of state regulatory strategies pre/post doctrine; measurement of whether doctrinal protection actually blocks majoritarian preference enactment or merely shifts regulatory form; tracking of litigation burden required to sustain protection',
    'If genuine constraint: suppression ~0.48 is appropriate. If state retains substantial pathways: suppression is lower (~0.25-0.35), constraint moves toward rope, doctrine is less extractive against majoritarian coalitions. If state can exhaust litigation capacity: suppression is higher (~0.60+), doctrine becomes partially phantom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of majoritarian coalitions is genuine or circumvented').

omega_variable(
    reading_grounding_philosophical_vs_doctrinal,
    'Is the privacy line reading grounded in philosophical claims about human dignity and autonomy (deontological), or in doctrinal/interpretive claims about constitutional text and precedent (conventional/lineage)?',
    'Analysis of the reading''s own self-justification: does it claim protection rests on natural personhood, or on the Constitution''s design? Do justices grounding the reading cite foundational rights language or textual interpretation?',
    'If philosophical grounding: axiom_status should be ''holdable'' (the foundational claim remains contestable across frameworks). If doctrinal grounding: axiom_status should reflect the stability of the doctrinal lineage — may be ''holdable'' or ''overridden'' depending on whether successor jurisprudence has abandoned the interpretive move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_grounding_philosophical_vs_doctrinal, conceptual, 'Philosophical vs doctrinal grounding of privacy reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substantive_due_process__privacy_line_reading, 0, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substantive_due_process__privacy_line_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(subs_tr_t1965, substantive_due_process__privacy_line_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(subs_tr_t1992, substantive_due_process__privacy_line_reading, theater_ratio, 1992, 0.42).
narrative_ontology:measurement(subs_tr_t2022, substantive_due_process__privacy_line_reading, theater_ratio, 2022, 0.38).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substantive_due_process__privacy_line_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(subs_be_t1965, substantive_due_process__privacy_line_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(subs_be_t1992, substantive_due_process__privacy_line_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(subs_be_t2022, substantive_due_process__privacy_line_reading, base_extractiveness, 2022, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substantive_due_process__privacy_line_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substantive_due_process__privacy_line_reading, substantive_due_process__history_tradition_test_reading).
narrative_ontology:affects_constraint(substantive_due_process__privacy_line_reading, substantive_due_process__lochner_economic_liberty_reading).

% DUAL FORMULATION NOTE:
% The privacy line reading is one interpretation of the substantive due process kernel. The other readings (history-tradition test, economic liberty) have different ε values reflecting different boundary positions and different victim sets. Each reading is a structurally distinct constraint. They are linked via the kernel (they all interpret the same constitutional text) but they instantiate different constraints with different extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
