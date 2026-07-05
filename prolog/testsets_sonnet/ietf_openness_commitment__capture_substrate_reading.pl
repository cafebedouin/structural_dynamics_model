% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Standards Process as Capture Substrate (Resource-Advantage Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the capture_substrate_reading of the
 *   ietf_openness_commitment kernel: the IETF's rough-consensus, running-code
 *   process is read here as a coordination substrate whose formally neutral
 *   procedure translates raw resource advantage (sustained paid engineering
 *   headcount, multi-year meeting attendance, ability to pre-coordinate among
 *   a vendor coalition) into encoded technical gatekeeping. The genuine
 *   coordination function (shared interoperable specifications) is real and
 *   is why this reading is tangled_rope rather than snare: independent
 *   implementers and end users do receive real interoperability benefit from
 *   having any open specification at all, even as the same mechanism
 *   structurally advantages whichever participants can outlast the drafting
 *   process. This is a distinct constraint from the
 *   commons_stewardship_reading (which treats the standard's openness itself
 *   as the primary fact, with capture as noise) and from the
 *   legitimacy_erosion_reading (which is about whether the rough-consensus
 *   mechanism's own legitimacy is failing, a meta-level question about the
 *   process rather than about resource-driven text capture). Each reading has
 *   its own ε and its own stakeholder structure; they are linked as siblings
 *   via network and cs_structure, not merged.
 *
 * KEY AGENTS:
 *   - large_platform_operators: primary structural beneficiary (institutional/arbitrage) — encodes infrastructure advantage into standard text
 *   - well_resourced_vendor_coalitions: secondary beneficiary (organized/mobile) — pre-coordinates consensus outcomes
 *   - independent_implementers: primary target (moderate/constrained) — bears the cost of asymmetric resourcing
 *   - small_isp_operators and end_users_of_locked_extensions: diffuse targets (powerless/trapped) — absorb downstream effects with no process visibility
 *   - ietf_leadership: agenda_setter/observer (institutional/analytical) — administers a formally neutral process that does not correct for resourcing asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.47).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Standards Process as Capture Substrate (Resource-Advantage Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '1397a922-956f-404f-a9cb-a7cd994b305b').
narrative_ontology:cs_kernel_codification('1397a922-956f-404f-a9cb-a7cd994b305b', distributed).
narrative_ontology:cs_authority_grounding('1397a922-956f-404f-a9cb-a7cd994b305b', practice).
narrative_ontology:cs_interpretation_layer_present('1397a922-956f-404f-a9cb-a7cd994b305b').
narrative_ontology:cs_reading_relation('1397a922-956f-404f-a9cb-a7cd994b305b', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('1397a922-956f-404f-a9cb-a7cd994b305b', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('1397a922-956f-404f-a9cb-a7cd994b305b', foundational, sustained_resourcing_functions_as_de_facto_veto).
narrative_ontology:cs_axiom_status(sustained_resourcing_functions_as_de_facto_veto, holdable).
narrative_ontology:cs_axiom_grounding('1397a922-956f-404f-a9cb-a7cd994b305b', sustained_resourcing_functions_as_de_facto_veto, empirically_contingent).
narrative_ontology:cs_axiom('1397a922-956f-404f-a9cb-a7cd994b305b', secondary, formal_procedural_openness_does_not_entail_substantive_access).
narrative_ontology:cs_axiom_status(formal_procedural_openness_does_not_entail_substantive_access, holdable).
narrative_ontology:cs_axiom_grounding('1397a922-956f-404f-a9cb-a7cd994b305b', formal_procedural_openness_does_not_entail_substantive_access, conventional).
narrative_ontology:cs_reference_frame('1397a922-956f-404f-a9cb-a7cd994b305b', rough_consensus_running_code_meritocracy).
narrative_ontology:cs_drift_state('1397a922-956f-404f-a9cb-a7cd994b305b', post_platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1397a922-956f-404f-a9cb-a7cd994b305b', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_isp_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users_of_locked_extensions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sends dozens of full-time paid engineers to working groups, chairs key groups, authors the majority of drafts, and can sustain multi-year attendance at every interim meeting. Proposes protocol extensions calibrated to interoperate cleanly with its own existing infrastructure while remaining merely 'compliant enough' with the open spec for competitors, effectively encoding its infrastructure advantage into the standard text itself. Can walk away from any given standard and ship a de facto proprietary alternative that market power makes the standard anyway.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary).

% Coordinates informally among a handful of large vendors before working group meetings to arrive at rough consensus already agreed among themselves, presenting a unified front that reads as organic technical consensus to less-resourced participants. Benefits from the same encoded advantages as platform operators without bearing the reputational cost of being the sole proposer.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, well_resourced_vendor_coalitions, beneficiary,
    organized, generational, mobile, global).

% Sends at most one engineer part-time, cannot attend every interim call, and must implement whatever the standard says even when informal side-channel agreements among larger vendors have already shaped the draft before it reaches public comment. Depends on interoperability with the standard for market access; cannot credibly threaten to fork because they lack the market share to make a fork stick.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_implementers, payer,
    moderate, biographical, constrained, global).

% Must deploy whatever the ratified standard specifies to remain interoperable with upstream and downstream networks, absorbing the integration cost of extensions written to favor equipment vendors with working-group presence. Has no seat at the table and no resources to send one; discovers proprietary-flavored extensions only after ratification.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_isp_operators, payer,
    powerless, biographical, trapped, regional).

% Experiences the practical effect of encoded gatekeeping as reduced interoperability between products nominally built on the same 'open' standard — features that only work correctly within one vendor's ecosystem. Has no visibility into the standards process and no mechanism to object; bears the switching costs and degraded functionality without ever entering the room where the extension was proposed.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users_of_locked_extensions, payer,
    powerless, immediate, trapped, global).

% Administers the rough-consensus process, chairs selection, and the appeals mechanism, but has limited tools to detect or correct for asymmetric resourcing among participants; procedural neutrality is maintained even when the underlying resource distribution predetermines whose drafts survive to last call.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, ietf_leadership, observer).

% Attends occasionally, raises interoperability and privacy concerns, but lacks the sustained engineering headcount to shepherd counter-drafts through the multi-year process; objections are procedurally heard but rarely translate into surviving text because sustained presence is what wins rough consensus in practice.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, academic_and_civil_society_participants, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, documented protocol specification so that independently built implementations can interoperate without bilateral negotiation between every pair of vendors — genuinely solves a real multi-party coordination problem that no single actor could solve alone.
% TRANSFER_FUNCTION: Moves technical control and de facto market position toward whichever participants can sustain the highest engineering headcount in the working-group process, at the expense of implementers and users who must accept whatever text results but had no proportionate voice in shaping it.
% ABSENT_VOICES: Academic and civil-society participants raise interoperability and privacy concerns but cannot sustain multi-year drafting presence; end users and small ISP operators are structurally absent from the room entirely and only encounter the standard's effects after ratification.
% DISAPPEARANCE_RATIONALE: If the IETF process vanished, large platform operators would likely retain de facto standardization power through market dominance and closed specifications, while independent implementers would lose the one forum where they currently have any formal voice at all, however diluted by resourcing asymmetry. Whether the world 'rearranges' or 'stays the same' depends on whether the coordination function or the gatekeeping function is treated as primary — which is exactly the contest this reading exists to isolate.
% FOUNDING_PROBLEM: Early internet protocol development risked fragmenting into incompatible vendor-specific implementations; a body was needed to produce voluntary, rough-consensus, running-code-tested specifications that any implementer could adopt without licensing gatekeeping.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and long-tenured participants attest the rough-consensus function still works as designed. Independent implementers, smaller vendors, and several academic observers (documented in workshop retrospectives and post-hoc analyses of specific working groups) attest that sustained-presence resourcing now functions as an informal but decisive gate that the original design did not anticipate and does not correct for.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.58 at interval end, rising from 0.35) because the mechanism is genuinely mixed: real interoperability coordination coexists with resource-driven text capture that has intensified as standards work has grown more technically complex and time-intensive over the interval, raising the effective cost of sustained participation and widening the gap between well-resourced and under-resourced participants. Suppression is moderate (0.47) — there is no formal barrier to participation (the process is open by design), but the practical suppression comes from the cost structure of sustained engagement, which functions as a soft barrier without needing formal exclusion rules. Theater ratio rises to 0.42, reflecting a growing share of procedural activity (public comment periods, open mailing lists) that preserves the appearance of open participation while the substantive drafting work happens in pre-coordinated vendor discussions that precede formal working-group sessions.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator's seat, the process looks like legitimate, hard-won rough consensus achieved through diligent sustained participation — an achievement, not a capture. From the independent implementer's seat, the same rough-consensus mechanism looks like a filter that systematically selects for whoever can afford to stay in the room longest. The engine computes both seats from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators and vendor coalitions sit near the beneficiary end: they set the agenda, shape the text, and can exit to proprietary alternatives if a standard doesn't suit them, giving them structural power even over the constraint's outcome. Independent implementers, small ISPs, and end users sit near the target end: they must accept whatever text results, cannot sustain the resourcing needed to shape it, and have limited or no exit (small ISPs are trapped by interoperability requirements; end users are trapped by product lock-in they cannot see the cause of). IETF leadership occupies an unusual dual position — institutional power but analytical exit options, since it administers rather than captures the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (shared interoperable specs) has not disappeared or gone dead — it remains genuinely live, which is precisely why this constraint is authored as tangled_rope rather than snare. Classifying it as pure extraction would erase the real value independent implementers and end users receive from having any open standard rather than pure vendor balkanization; classifying it as pure coordination (rope) would erase the documented asymmetry in whose drafts survive to ratification. The tangled_rope classification is the only one of the six that requires both a genuine coordination function AND identifiable asymmetric extraction to be named simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_stewardship_primacy,
    'Is resource-driven text capture the primary structural fact about the IETF process (this reading), or is open-standard preservation the primary fact with capture as a secondary, correctable defect (the sibling commons_stewardship_reading)?',
    'Comparative analysis across working groups: measure the correlation between sponsor engineering headcount and survival rate of proposed text to last call, across a large sample of standards. A strong, consistent correlation supports this reading as primary; a weak or absent correlation across most working groups (with capture concentrated in a few high-stakes groups) would support treating stewardship as primary and capture as localized noise.',
    'If capture correlation is weak and localized, this reading''s extractiveness estimate is too high relative to the corpus as a whole and should be revised toward a rope classification with isolated tangled-rope exceptions rather than a general tangled-rope reading of the whole process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_stewardship_primacy, empirical, 'Whether capture or stewardship is the primary structural reading of the same kernel.').

omega_variable(
    process_neutrality_vs_engineered_outcome,
    'Is the rough-consensus process''s failure to correct for resourcing asymmetry a designed feature (the process was never meant to equalize resourcing, only to require technical justification) or a design failure relative to the process''s own stated openness commitments?',
    'Historical analysis of IETF founding documents and RFC 3935 (mission statement) against the actual resourcing patterns of high-impact working groups over multiple decades.',
    'If the asymmetry is a designed feature rather than a failure, the ''gatekeeping'' framing in this reading is less an indictment of the process and more a description of an intentional meritocratic filter — which would push this reading''s claimed_type toward rope. If it is a genuine departure from founding commitments, tangled_rope or even a harder capture reading is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(process_neutrality_vs_engineered_outcome, conceptual, 'Whether encoded resourcing advantage is a design feature or a departure from the process''s own openness commitments.').

omega_variable(
    sibling_kernel_reading_boundary,
    'Where exactly does this reading''s claim (text-level capture via resourcing) stop and the legitimacy_erosion_reading''s claim (institutional trust in the rough-consensus mechanism itself) begin?',
    'Track whether critiques in the field target specific standards'' text (supports this reading as the operative frame) versus target the legitimacy of the consensus-calling process itself, e.g. disputes over chair discretion in declaring consensus (supports the sibling reading as the operative frame).',
    'Clarifies which reading a given empirical critique should be filed under, preventing double-counting of the same evidence across both sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_kernel_reading_boundary, conceptual, 'Boundary condition distinguishing this reading from the legitimacy_erosion_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ietf_openness_commitment kernel, decomposed per the ε-invariance principle: capture_substrate_reading (this story, tangled_rope, moderate-rising ε), commons_stewardship_reading (open-standard preservation as primary fact, lower ε, likely rope), and legitimacy_erosion_reading (meta-level question about the rough-consensus mechanism's own legitimacy under organized capture pressure, a distinct claim about institutional trust rather than text-level outcomes). All three share the same underlying institutional substrate (the IETF standards process) but instantiate structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications. They are linked bidirectionally via affects_constraints; each carries its own cs_structure.reading_relations and axioms documenting the specific structural relationship to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
