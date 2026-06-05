% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_judicial_ambiguity, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Length: Judicial Ambiguity Reading (Judicial Deference Enables Congressional Discretion)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   Copyright term length in U.S. law represents a foundational conflict
 *   between constitutional limits (Article I, Section 8 grants Congress power
 *   over copyright 'for limited Times') and institutional practice (Congress
 *   repeatedly extends copyright terms, courts defer via rational basis
 *   review). This constraint story instantiates ONE reading of that conflict:
 *   the judicial_ambiguity_reading, which frames the constraint as a
 *   tangled_rope in which judicial deference to Congress creates both
 *   coordination (clear copyright rules enabling licensing markets) and
 *   extraction (the 'limited Times' constitutional requirement is
 *   functionally disabled). The reading contrasts with two siblings: the
 *   public_scaffold_reading (which sees heightened scrutiny as an emerging
 *   doctrinal exit path) and the corporate_enclosure_reading (which frames
 *   copyright extension as pure snare serving only corporate interests). This
 *   reading's core claim is that judicial rationality review enables a stable
 *   arrangement where Congress retains discretion to extend terms
 *   indefinitely, courts are prevented from enforcing constitutional limits,
 *   copyright-holding corporations benefit, and the public domain is trapped.
 *   The constraint's extractiveness (0.38) is moderate—not as severe as pure
 *   corporate enclosure (which would score 0.65+) but substantially higher
 *   than pure coordination (which would score <0.15). The theater_ratio
 *   (0.58) reflects that copyright-term legislation is presented as normal
 *   lawmaking, not as a creative constitutional reinterpretation, even though
 *   the repeated extensions represent a doctrinal shift from 'limited' to de
 *   facto unlimited.
 *
 * KEY AGENTS:
 *   - Congress: Primary beneficiary (institutional/arbitrage) — retains unrestricted discretion to extend copyright terms; rational basis review forecloses judicial invalidation
 *   - Copyright-Holding Corporations: Secondary beneficiary (institutional/arbitrage) — benefit from extended terms and indefinite copyright protection; can arbitrage by licensing to competitors
 *   - Federal Courts: Trapped institutional actor (institutional/constrained) — bound by rational basis precedent that prevents enforcement of 'limited Times' constitutional language; benefit from clear deference doctrine but constrained from interpreting the Constitution they are sworn to uphold
 *   - Public Domain Access Coalition: Primary victim (organized/constrained) — organized actors (libraries, archives, creative commons advocates) face constrained exit from copyright regime; litigation has failed due to rational basis deference
 *   - Public Domain Itself (Powerless): Structural victim (powerless/trapped) — the constitutional requirement of 'limited Times' is victimized by judicial doctrine that permits indefinite de facto extension; no mechanism to enforce the constitutional limit
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent doctrinal choice (rational basis review) as a law of democratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Length: Judicial Ambiguity Reading (Judicial Deference Enables Congressional Discretion)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d').
narrative_ontology:cs_kernel_codification('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', fixed_text).
narrative_ontology:cs_authority_grounding('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', extraction).
narrative_ontology:cs_interpretation_layer_present('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d').
narrative_ontology:cs_reading_relation('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', foundational, judicial_deference_maintains_constitutional_constraint_enforcement).
narrative_ontology:cs_axiom_status(judicial_deference_maintains_constitutional_constraint_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', judicial_deference_maintains_constitutional_constraint_enforcement, conventional).
narrative_ontology:cs_axiom('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', foundational, copyright_coordination_benefit_coexists_with_extraction_asymmetry).
narrative_ontology:cs_axiom_status(copyright_coordination_benefit_coexists_with_extraction_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', copyright_coordination_benefit_coexists_with_extraction_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', constitutional_limited_times_enforcement).
narrative_ontology:cs_drift_state('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28e0e72e-8be1-42ba-98b2-d10ef8a9ea5d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_corporations).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_constraint).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DOMAIN ACCESS (SNARE) — Citizens seeking creative works to enter the public domain face no realistic exit from copyright extension. Works extended through Sonny Bono Act (Life+70) trap cultural material indefinitely. No alternative mechanism to access expired copyrights. Cannot organize meaningful resistance; the doctrinal gate (rational basis deference) forecloses judicial redress. Maximum suppression, no coordination benefit.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPEN ACCESS COALITION (TANGLED ROPE) — Libraries, archives, and open-access advocates face constrained exit (litigation has failed; legislative change is difficult but possible). They benefit from the copyright system's coordination function (clarifying ownership and enabling licensing deals for some materials) while bearing extraction costs (indefinite extensions, high licensing fees, restricted access). The constraint enables them to negotiate some licensing but forecloses full public-domain restoration without legislative action.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS (ROPE) — Congressional authority benefits from judicial deference to copyright legislation. The rational basis test permits Congress to extend copyright terms without constitutional constraint. Congress experiences the constraint as pure coordination: it provides a clear rule (copyright extends for Life+70 years) that enables licensing markets and content production coordination. No extraction cost to Congress; the entire institutional benefit accrues to this actor. Arbitrage option: Congress can adjust copyright terms freely within rational basis review.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COPYRIGHT-HOLDING CORPORATIONS (ROPE) — Large media and entertainment corporations benefit from extended copyright terms. The judicial deference framework ensures term extensions are treated as legitimate exercises of congressional power. Corporations experience this as pure coordination (copyright law provides clear ownership rules enabling licensing and enforcement). They can exit via arbitrage: license materials to competitors, lobby for further extensions, or work with Congress to adjust terms. Net beneficiary; no significant extraction cost.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL COURTS (TANGLED ROPE) — Courts benefit from a clear doctrinal rule (rational basis deference) that reduces decision-making burden. But they are constrained by the constitutional text they are supposed to enforce — Article I, Section 8 grants Congress power over copyright 'for limited Times,' yet rational basis review permits indefinite de facto extension. Courts experience this as a mixed constraint: coordination (clear deference doctrine simplifies adjudication) but also extraction (the courts are prevented from enforcing the constitutional textual limit). Constrained exit: courts could apply heightened scrutiny, but institutional legitimacy cost is high; the precedent of rational basis review is deeply embedded.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, legislative authority over copyright is immutable: democracies necessarily grant legislatures discretion over property rules, and judicial interference with legislative classification schemes risks institutional breakdown. Courts cannot second-guess rational judgments. This perspective frames judicial deference as a structural law of democratic governance. However, the structural data reveals false-summit dynamics: the beneficiaries (Congress, corporations) have clear institutional interests in broad deference; the victimization (public domain, constitutional fixity) is real and measurable. The 'law of legislative supremacy' naturalizes what is actually a contingent institutional arrangement shaped by specific doctrinal choices.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__judicial_ambiguity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts benefit for Congress and corporations (longer copyright terms, stronger enforcement mechanisms) from the public (delayed or prevented access to works that would otherwise enter public domain). The extraction is not total because copyright law also provides coordination benefits (clear ownership rules, enabling legitimate licensing markets, incentivizing original creation). The value reflects that judicial deference enables congressional discretion without clear limits, shifting the balance from 'limited times' toward indefinite protection, but the system is not purely predatory — it coordinates legitimate market activity. Suppression (0.45): Moderate-high. Citizens cannot easily challenge copyright extensions through litigation (rational basis review defeats most constitutional claims). Legislative change is difficult (copyright industries lobby effectively). But suppression is not total — public-domain advocacy exists, some academic and library movements are gaining traction, and Congress is occasionally responsive. Theater ratio (0.58): Moderate-high. Copyright-term legislation is presented as straightforward IP policy updates, not as constitutional reinterpretation. The Sonny Bono Act (1998) was framed as simple term extension, not as a doctrinal shift that eliminated the 'limited Times' constitutional requirement. The theater has increased over time as extensions have accumulated without justification, and academic critique has grown, yet the legislation continues to be enacted as if it remains within legitimate constitutional bounds.
 *
 * PERSPECTIVAL GAP:
 *   The judicial_ambiguity_reading produces perspectival gaps across institutional and powerless agents. Congress and copyright corporations see rope (pure coordination benefiting them through clear rules and extended protection). Courts see tangled_rope (coordination benefit from clear deference doctrine, but extraction cost from being unable to enforce the Constitution they interpret). The open-access coalition sees tangled_rope (mixed coordination and extraction; some licensing access but indefinite term extension). The public-domain aspirant sees snare (trapped by rational basis review, no alternative mechanism, maximum suppression). The analytical observer risks mountain classification (legislative supremacy is an immutable law of democracy) but the structural data reveals false-summit dynamics: the 'law' naturalizes a specific doctrinal choice that benefits identifiable institutional actors. The perspectival gap reveals that judicial deference is not inevitable but contingent—it could shift if courts applied heightened scrutiny or if Congress faced political pressure for reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) derives from their structural position relative to the constraint. Congress as primary beneficiary with unrestricted arbitrage option (can extend terms freely) gets low d (~0.10), producing negative f(d), meaning Congress experiences negative effective extraction (they are the extraction beneficiary). Copyright corporations as institutional beneficiary with arbitrage option get low d (~0.15), also negative f(d). The open-access coalition as organized victim with constrained exit gets moderate d (~0.50), producing moderate f(d), reflecting mixed extraction. The public-domain aspirant as powerless victim with trapped exit gets high d (~0.90), producing high f(d) (~1.30), reflecting maximum experienced extraction. Federal courts as institutional actor with constrained exit (bound by precedent) get moderate-high d (~0.55), reflecting that courts are partially victimized by the doctrine they must enforce. The analytical observer at the observation position gets canonical d for analytical (0.72), permitting view of the full structure but at risk of naturalizing the institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The judicial_ambiguity_reading resolves mandatrophy by showing that tangled_rope is the correct classification when judicial deference is understood as a contingent choice rather than a natural law. The constraint has genuine coordination function (copyright law clarifies ownership and enables licensing markets), but judicial doctrine prevents enforcement of the constitutional 'limited Times' requirement, creating extraction (indefinite copyright protection traps the public domain). The mandatrophy is resolved by distinguishing: (1) the coordination function is real and necessary, (2) the extraction asymmetry is also real and measurable, (3) the judicial deference doctrine is contingent and could shift, (4) the constraint is therefore tangled_rope (hybrid coordination/extraction) rather than pure rope (coordination only) or snare (extraction only). The reading accepts that Congress has legitimate authority over copyright policy but argues that rational basis review has disabled the constitutional constraint of 'limited Times,' creating an asymmetric arrangement where beneficiaries profit indefinitely and victims cannot access public-domain materials. The resolution depends on whether courts maintain rational basis deference (supporting tangled_rope classification) or shift to heightened scrutiny (shifting toward scaffold reading, where the constraint is temporary and has a doctrinal sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_basis_ceiling_ambiguity,
    'Does rational basis review permit any meaningful judicial constraint on copyright term length, or has the doctrine functionally eliminated Article I, Section 8''s ''limited Times'' requirement?',
    'Empirical analysis of judicial precedent: identify any case in which rational basis review rejected a copyright term as unconstitutionally unlimited. If no such cases exist, the constitutional constraint is functionally foreclosed by doctrine.',
    'If rational basis permits no constraint: the ''limited Times'' language is purely ceremonial (mountain from the analytical view is correctly classified). If rational basis could permit constraint but hasn''t yet been applied: the doctrine is under-determined and could shift with new precedent (tangled_rope classification is more accurate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_basis_ceiling_ambiguity, empirical, 'Whether rational basis review permits any meaningful constraint on copyright term length').

omega_variable(
    doctrinal_change_mechanism,
    'What constellation of events or actors could shift courts from rational basis review to heightened scrutiny for copyright terms?',
    'Historical analysis of scrutiny-level shifts in First Amendment and property doctrine. Identify triggering conditions: political pressure, empirical evidence of harm, ideological shifts in judicial composition, changes in institutional conditions affecting courts'' credibility.',
    'If mechanism identified and likely: scaffold reading is structurally sound — the constraint has a latent exit path. If mechanism is absent or highly improbable: the constraint approaches true mountain status via institutional lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_change_mechanism, conceptual, 'Conditions for shifting from rational basis to heightened scrutiny in copyright doctrine').

omega_variable(
    reading_selector_ambiguity,
    'Is this judicial_ambiguity_reading (deference enables congressional discretion) or the public_scaffold_reading (deference is temporary, heightened scrutiny is emerging) the more accurate framing?',
    'Monitor judicial opinions for evidence of dissent from rational basis doctrine, law review scholarship frequency and citation patterns, amicus briefs in copyright cases, congressional responsiveness to public-domain advocacy. Rising scholarly critique and congressional attention shift probability toward scaffold reading.',
    'If judicial_ambiguity dominates: the constraint is tangled_rope (courts trapped; Congress benefits). If scaffold reading dominates: the constraint approaches temporary_support (sunset via doctrine shift). If corporate_enclosure reading dominates: the constraint is pure snare (extraction accelerating).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selector_ambiguity, empirical, 'Which sibling reading more accurately describes the constraint''s current state').

omega_variable(
    constitutional_fixity_victim_status,
    'Is the constitutional ''limited Times'' language a genuine constraint (with standing to be victimized by circumvention), or a dead letter whose violation is not actionable?',
    'Examine standing doctrine for constitutional copyright claims. Does Article I, Section 8 ''limited Times'' language have a plausible injury-in-fact? Can citizens sue on the theory that their constitutional right to benefit from a public domain has been violated?',
    'If standing is recognized: constitutional fixity is a real victim (constraint has genuine structural asymmetry). If standing is denied: constitution is unenforceable against Congress in this domain (rational basis has foreclosed constitutional limits entirely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_fixity_victim_status, empirical, 'Whether constitutional ''limited Times'' language has enforceable standing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_sonny_bono_1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theater_mid_2010s, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(theater_contemporary, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(base_extract_sonny_bono_1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(base_extract_mid_2010s, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(base_extract_contemporary, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppress_sonny_bono_1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(suppress_mid_2010s, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(suppress_contemporary, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, patent_term_extension_rational_basis).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, trademark_indefinite_renewal).

% DUAL FORMULATION NOTE:
% The copyright_constitutional_mandate kernel has three constraint stories corresponding to three distinct readings. The judicial_ambiguity_reading focuses on how rational basis review enables congressional discretion without clear constitutional limits. The public_scaffold_reading frames heightened scrutiny as an emerging doctrinal exit. The corporate_enclosure_reading argues the coordination function is illusory. All three stories share the same constitutional text but arrive at different structural analyses. They are linked via network.affects_constraints to show constraint family membership and are differentiated by their cs_structure.reading_relations and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
