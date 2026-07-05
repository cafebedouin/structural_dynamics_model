% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation as Commons-Preservation Institution
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the GPL reciprocity kernel:
 *   copyleft as institutional technology that prevents commons enclosure. On
 *   this reading, the beneficiary of the mandatory reciprocity clause is not
 *   any individual actor but the software commons as a persisting
 *   institution, and the primary victims are individual exit-maximizers who
 *   would otherwise extract value from community contributions and
 *   permanently remove it from circulation. This reading treats the
 *   obligation's coercive character (you must release derivative source if
 *   you distribute) as the necessary enforcement cost of maintaining a
 *   genuine, non-enclosable shared resource — hence tangled_rope: real
 *   coordination function (sustained shared infrastructure) plus real,
 *   asymmetric extraction (from those who would have preferred to enclose).
 *   Two sibling readings of the same GPL reciprocity kernel are NOT part of
 *   this story: copyleft_as_freedom_reading (which frames the beneficiary as
 *   the individual end-user's freedom rather than the commons-as-institution)
 *   and copyleft_as_restriction_reading (which frames the same clause
 *   primarily as a constraint on business models, with commercial firms as
 *   the salient victim class and no institutional beneficiary named). Each of
 *   those is a separate constraint with its own ε and its own stakeholder
 *   structure; they are linked here only via network.affects_constraints, not
 *   folded into this ε.
 *
 * KEY AGENTS:
 *   - the_software_commons: institutional beneficiary (non-agent) — the persisting resource whose non-enclosability is the coordination target
 *   - long_horizon_maintainers: agenda_setter/beneficiary (organized/constrained) — write, administer, and depend on the reciprocity mechanism
 *   - individual_exit_maximizers: primary payer (moderate/trapped) — blocked from the enclosure path they would otherwise pursue
 *   - proprietary_forkers: secondary payer (powerful/constrained) — larger firms with more resources to route around the obligation but still structurally blocked where components are load-bearing
 *   - compliance_enforcement_bodies: agenda_setter (organized/analytical) — the active enforcement mechanism without which reciprocity would be aspirational only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.5).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation as Commons-Preservation Institution").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '1c9d5359-6624-469c-80c2-bbd16b93640b').
narrative_ontology:cs_kernel_codification('1c9d5359-6624-469c-80c2-bbd16b93640b', fixed_text).
narrative_ontology:cs_authority_grounding('1c9d5359-6624-469c-80c2-bbd16b93640b', lineage).
narrative_ontology:cs_interpretation_layer_present('1c9d5359-6624-469c-80c2-bbd16b93640b').
narrative_ontology:cs_reading_relation('1c9d5359-6624-469c-80c2-bbd16b93640b', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c9d5359-6624-469c-80c2-bbd16b93640b', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('1c9d5359-6624-469c-80c2-bbd16b93640b', foundational, commons_persistence_outweighs_individual_capture_right).
narrative_ontology:cs_axiom_status(commons_persistence_outweighs_individual_capture_right, holdable).
narrative_ontology:cs_axiom_grounding('1c9d5359-6624-469c-80c2-bbd16b93640b', commons_persistence_outweighs_individual_capture_right, instrumental).
narrative_ontology:cs_axiom('1c9d5359-6624-469c-80c2-bbd16b93640b', secondary, reciprocity_obligation_is_legitimate_enforcement_cost_of_shared_infrastructure).
narrative_ontology:cs_axiom_status(reciprocity_obligation_is_legitimate_enforcement_cost_of_shared_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('1c9d5359-6624-469c-80c2-bbd16b93640b', reciprocity_obligation_is_legitimate_enforcement_cost_of_shared_infrastructure, conventional).
narrative_ontology:cs_reference_frame('1c9d5359-6624-469c-80c2-bbd16b93640b', hacker_sharing_culture_reciprocity_norm).
narrative_ontology:cs_drift_state('1c9d5359-6624-469c-80c2-bbd16b93640b', contemporary_corporate_foss_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1c9d5359-6624-469c-80c2-bbd16b93640b', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_forkers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, end_users_of_derivative_products).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_are_defensible_against_enclosure).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, reciprocity_obligations_sustain_shared_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated body of GPL-licensed code that persists and grows because every derivative work that gets distributed must also be released under the same reciprocal terms. It is not an actor that can defend itself; it depends entirely on the license's copyleft mechanism and on maintainers and the FSF/enforcement bodies choosing to enforce it. Its 'benefit' is structural: contributions cannot be siphoned out permanently into closed derivatives that never return value.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons).

% Core maintainers and foundations (e.g. FSF, Conservancy-style bodies) who wrote and interpret the license, pursue compliance actions, and administer the reciprocity obligation. They benefit from a durable commons they can build on indefinitely, but they also bear the ongoing cost of enforcement litigation and community management, and their exit from the license regime they've built would mean abandoning the project's governance model entirely.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_horizon_maintainers, beneficiary).

% Developers who build on GPL code and contribute improvements back. They gain a large body of freely reusable software and confidence that others' improvements will likewise return to the pool, but they must also release their own derivative source, which forecloses certain commercial packaging strategies they might otherwise have pursued.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_contributors, payer).

% Developers or small firms who want to take GPL code, modify it, and distribute a closed derivative to capture proprietary value without reciprocating. The reciprocity obligation directly blocks this path: any distribution triggers source-disclosure and same-license requirements. Their only real exits are not using GPL code at all (sacrificing its utility) or negotiating a separate commercial license from the copyright holders, which is often unavailable for community-authored code with many contributors.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    moderate, biographical, trapped, national).

% Larger commercial entities that would prefer to absorb GPL components into proprietary products without redistributing source. They have more resources to seek dual-licensing deals or to route around GPL dependencies entirely, but where a GPL component is load-bearing and irreplaceable, they are structurally blocked from enclosure and must either comply, rewrite the component, or avoid distributing the derivative.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_forkers, payer,
    powerful, biographical, constrained, global).

% Organizations that monitor for license violations, issue compliance demands, and litigate when necessary. They are the active mechanism that makes the reciprocity obligation more than aspirational — without enforcement capacity the obligation would be honored only by the already-cooperative.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, compliance_enforcement_bodies, agenda_setter,
    organized, generational, analytical, global).

% People who ultimately run software built on GPL-licensed components. They benefit indirectly from the commons remaining open — more auditable, forkable, repairable software exists than would under a purely proprietary regime — but have no direct role in negotiating or enforcing the license terms themselves.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, end_users_of_derivative_products, beneficiary,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of shared software infrastructure: without a reciprocity requirement, any contributor could take community-improved code, close it off, and capture the surplus, which would rationally deter future contribution. The obligation makes contributing to the commons individually rational because contributions cannot be permanently extracted out of the pool.
% TRANSFER_FUNCTION: Moves the right to enclose derivative works away from individual distributors and toward the commons as a whole: whoever distributes a GPL derivative must return the source and license terms to the same pool they drew from, rather than capturing exclusive downstream value.
% ABSENT_VOICES: Commercial entities who would have built proprietary products on top of freely available code, capturing more concentrated profit, are structurally prevented from making that case inside the license's own terms — they can only argue it in courts, in alternative licensing markets, or by declining to use GPL code at all. Their objection ('this constrains legitimate business models') is heard elsewhere, not adjudicated by the license itself.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, commercially motivated actors would rapidly fork and close large parts of the current commons, since nothing would require returning derivative improvements. Contribution incentives for volunteer and organization maintainers would weaken once the closing-off pathway reopened, and much of the currently-open ecosystem (kernels, compilers, core libraries) would fragment into proprietary variants within a few product cycles.
% FOUNDING_PROBLEM: Early proprietary software vendors were observed capturing freely shared code, improving it privately, and distributing closed binaries with no obligation to return improvements — undermining the sharing culture research and hobbyist communities had built. The GPL was constructed specifically to make that enclosure path unavailable by attaching the reciprocity condition to the copyright grant itself.
% FOUNDING_PROBLEM_CORROBORATION: Corporate legal departments that comply with or route around GPL obligations (outside the FSF/maintainer beneficiary set) attest, in their own compliance guidance and licensing-strategy documents, that the obligation remains an active constraint on enclosure strategies today — not merely a historical artifact. Academic studies of open-source ecosystem health by researchers unaffiliated with any single foundation also document continuing attempts at enclosure that copyleft terms actively block, corroborating that the founding problem persists rather than having been resolved.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at medium (0.42) because the obligation does impose a real, binding cost on distributors who would prefer to enclose derivatives — this is genuine extraction from their preferred strategy, not merely friction. It is not authored high because the extraction is bounded (avoidable by not distributing GPL-derived code, or by negotiating alternate licenses from all rightsholders) and because the extracted value returns to a shared pool rather than to a concentrated private beneficiary. Suppression sits at 0.5 — moderate-high — because compliance is actively enforced through legal demand and litigation threat, not merely social pressure, but enforcement bodies generally pursue disclosure/compliance rather than punitive extraction, and voluntary compliance is common. Theater ratio is low (0.12) because compliance enforcement substantially tracks the real function (preventing enclosure) rather than performing activity disconnected from it. Accessibility collapse is authored at 0.6: once a project has substantial GPL-derived history with many contributors, negotiating an escape (relicensing) becomes practically very difficult, though not impossible for single-copyright-holder situations.
 *
 * PERSPECTIVAL GAP:
 *   From the long_horizon_maintainer seat, the reciprocity clause looks like genuine coordination technology they built and depend on. From the individual_exit_maximizer or proprietary_forker seat, the identical clause looks like an enforced extraction blocking their preferred strategy. The engine should compute these as structurally different seat experiences of the same ε and structural data — the claim (tangled_rope) is intended to hold both truths simultaneously rather than resolve them into one.
 *
 * DIRECTIONALITY LOGIC:
 *   The_software_commons and long_horizon_maintainers are declared beneficiaries: the constraint subsidizes their position by guaranteeing that contributed value cannot be permanently removed from the pool. Individual_exit_maximizers and proprietary_forkers are declared victims: the reciprocity clause directly blocks their preferred value-capture strategy and its enforcement mechanism (compliance demands, litigation) is aimed specifically at them. Downstream_contributors and end_users occupy a more symmetric position — they benefit from commons durability but also bear the same reciprocity constraint if they redistribute, so they carry dual roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary vendors enclosing freely shared community code with no return obligation) remains live by external corroboration (corporate legal guidance, independent ecosystem research), not merely by the say of the beneficiary institutions themselves — this blocks a mandatrophy misreading where the obligation would be an inertial holdover from a solved problem. Because the founding problem stays live and the disappearance verdict is world_rearranges (not world_unchanged), this constraint should NOT be read as a piton: there is a concentrated administering party (maintainers/enforcement bodies) and an ongoing coordination function that active enforcement continues to serve, not empty theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_personification_ambiguity,
    'Is ''the software commons'' a coherent beneficiary entity whose interests can be meaningfully distinguished from the interests of the maintainers and foundations who administer it, or is ''the commons benefits'' simply a legitimating gloss on maintainer/foundation institutional interest?',
    'Examine cases where maintainer/foundation enforcement decisions diverge from what would most benefit ecosystem-wide code availability (e.g. strategic litigation chosen for precedent value over ecosystem health) — divergence would indicate the commons framing is partly a legitimating gloss.',
    'If the commons and the administering institutions'' interests are shown to diverge systematically, some of this story''s claimed beneficiary (the commons) collapses into a narrower institutional beneficiary, pushing the classification closer to a snare read from the exit-maximizer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_personification_ambiguity, conceptual, 'Whether the commons is a real beneficiary distinct from its administering institutions.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Among the three declared readings of the GPL reciprocity kernel (commons, freedom, restriction), which reading a given dispute invokes is itself contested — a single license-enforcement action could be simultaneously described by its proponents as commons-preservation and by its target as business-model restriction, with no framework-external fact settling which reading is ''correct'' for that instance.',
    'Track how courts and community governance bodies characterize enforcement rationale in specific compliance actions over time — a shift in official framing language would indicate which reading is institutionally ascendant, though this would not resolve which reading is structurally true.',
    'If the restriction reading becomes institutionally dominant in enforcement rhetoric, the commons reading''s claimed coordination function (this story) would face pressure even though its ε and stakeholder structure remain independently authored and unaffected by that rhetorical shift, per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the commons framing is the dominant or merely one available reading of enforcement actions.').

omega_variable(
    single_rightsholder_exit_asymmetry,
    'Does the practical availability of relicensing (for single-copyright-holder or dual-licensed projects) versus its near-impossibility (for many-contributor projects like the Linux kernel) mean this constraint''s accessibility_collapse value should differ sharply by project structure rather than being authored as one story-level scalar?',
    'Compare relicensing outcomes across a sample of single-holder vs. many-contributor GPL projects to establish whether accessibility_collapse is bimodal rather than continuous.',
    'If bimodal, this story''s single accessibility_collapse value (0.6) is an average masking two distinct sub-populations, and per the ε-invariance principle these might warrant decomposition into separate stories by project-contributor-structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_rightsholder_exit_asymmetry, empirical, 'Whether accessibility collapse varies structurally by contributor concentration rather than being a single scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement_basis(gpl__tr_t1989, observed).
narrative_ontology:measurement(gpl__tr_t1998, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1998, 0.07).
narrative_ontology:measurement_basis(gpl__tr_t1998, observed).
narrative_ontology:measurement(gpl__tr_t2005, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement_basis(gpl__tr_t2005, observed).
narrative_ontology:measurement(gpl__tr_t2012, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t2012, observed).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.11).
narrative_ontology:measurement_basis(gpl__tr_t2019, observed).
narrative_ontology:measurement(gpl__tr_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2025, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.2).
narrative_ontology:measurement_basis(gpl__be_t1989, observed).
narrative_ontology:measurement(gpl__be_t1998, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement_basis(gpl__be_t1998, observed).
narrative_ontology:measurement(gpl__be_t2005, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement_basis(gpl__be_t2005, observed).
narrative_ontology:measurement(gpl__be_t2012, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2012, 0.37).
narrative_ontology:measurement_basis(gpl__be_t2012, observed).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement_basis(gpl__be_t2019, observed).
narrative_ontology:measurement(gpl__be_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(gpl__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement_basis(gpl__su_t1989, observed).
narrative_ontology:measurement(gpl__su_t1998, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement_basis(gpl__su_t1998, observed).
narrative_ontology:measurement(gpl__su_t2005, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement_basis(gpl__su_t2005, observed).
narrative_ontology:measurement(gpl__su_t2012, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2012, 0.47).
narrative_ontology:measurement_basis(gpl__su_t2012, observed).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.49).
narrative_ontology:measurement_basis(gpl__su_t2019, observed).
narrative_ontology:measurement(gpl__su_t2025, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(gpl__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the same GPL reciprocity kernel, decomposed per the ε-invariance principle: copyleft_as_commons_reading (this file, tangled_rope, ε=0.42, beneficiary=commons-as-institution), copyleft_as_freedom_reading (expected rope-leaning, lower ε, beneficiary=end-user freedom), and copyleft_as_restriction_reading (expected snare-leaning from the commercial-actor seat, higher ε, victim=commercial business models broadly with no institutional beneficiary named). All three share the identical license clause but differ in beneficiary structure and consequently in ε and claimed_type. They are linked bidirectionally via affects_constraints rather than merged, since merging would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
