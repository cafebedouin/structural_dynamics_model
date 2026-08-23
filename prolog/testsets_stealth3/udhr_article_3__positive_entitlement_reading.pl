% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 - Positive Entitlement Reading (State Material-Provision Obligation)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the positive-entitlement reading of UDHR Article
 *   3: that 'security of person' obligates the state to provide the material
 *   conditions - welfare, healthcare, housing - necessary for life. The
 *   standing arrangement under contest, and therefore the epsilon referent,
 *   is the constitutionalized material-provision obligation itself as this
 *   reading assesses it - not the laissez-faire arrangement the
 *   negative-liberty sibling prefers, and not an idealized welfare regime. By
 *   this reading's own lights a large share of the compelled transfer counts
 *   as obligation fulfillment rather than taking, which is why epsilon sits
 *   at 0.58 rather than the markedly higher value a negative-liberty author
 *   would assign over the identical referent; epsilon is reading-indexed over
 *   a fixed referent. The arrangement coordinates genuinely (risk pooling,
 *   public-health externalities, adverse-selection failures in private
 *   insurance) while extracting asymmetrically (compulsory funding from
 *   property holders; bundled dignity-based speech limits borne by expression
 *   rights holders) under continuous enforcement. This file is one member of
 *   a three-story constraint family; the siblings author their own epsilon
 *   over the same kernel text.
 *
 * KEY AGENTS:
 *   - constitutional_welfare_state: agenda setter (institutional/constrained) - administers and enforces the provision obligation through taxation and program machinery
 *   - low_income_vulnerable_groups: primary beneficiary (powerless/trapped) - receives the material floor; individually leverless, latently an electoral coalition
 *   - property_holders_high_earners: primary target (powerful/constrained) - funds provision through compulsory taxation; partial capital arbitrage, no full exit
 *   - expression_rights_holders: secondary target (moderate/constrained) - bears bundled dignity-based speech limits; speech cannot arbitrage across borders
 *   - public_welfare_bureaucracy: secondary beneficiary and administrator (organized/identity_locked) - retains administrative budgets; fused with its function
 *   - private_provision_providers: excluded voice (organized/mobile) - crowded-out alternative providers with no seat in entitlement debates
 *   - treaty_monitoring_bodies: analytical observer (institutional/analytical) - interprets minimum-core content and documents retrogression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.58).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.5).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 - Positive Entitlement Reading (State Material-Provision Obligation)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '33e3710a-69dc-4793-a7ac-8a8228abd1ed').
narrative_ontology:cs_kernel_codification('33e3710a-69dc-4793-a7ac-8a8228abd1ed', fixed_text).
narrative_ontology:cs_authority_grounding('33e3710a-69dc-4793-a7ac-8a8228abd1ed', lineage).
narrative_ontology:cs_interpretation_layer_present('33e3710a-69dc-4793-a7ac-8a8228abd1ed').
narrative_ontology:cs_reading_relation('33e3710a-69dc-4793-a7ac-8a8228abd1ed', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('33e3710a-69dc-4793-a7ac-8a8228abd1ed', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('33e3710a-69dc-4793-a7ac-8a8228abd1ed', foundational, material_security_is_state_owed_duty).
narrative_ontology:cs_axiom_status(material_security_is_state_owed_duty, holdable).
narrative_ontology:cs_axiom_grounding('33e3710a-69dc-4793-a7ac-8a8228abd1ed', material_security_is_state_owed_duty, deontological).
narrative_ontology:cs_axiom('33e3710a-69dc-4793-a7ac-8a8228abd1ed', secondary, redistribution_fulfills_rather_than_violates_rights).
narrative_ontology:cs_axiom_status(redistribution_fulfills_rather_than_violates_rights, holdable).
narrative_ontology:cs_axiom_grounding('33e3710a-69dc-4793-a7ac-8a8228abd1ed', redistribution_fulfills_rather_than_violates_rights, instrumental).
narrative_ontology:cs_reference_frame('33e3710a-69dc-4793-a7ac-8a8228abd1ed', positive_obligation_material_security).
narrative_ontology:cs_drift_state('33e3710a-69dc-4793-a7ac-8a8228abd1ed', contemporary_austerity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('33e3710a-69dc-4793-a7ac-8a8228abd1ed', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, low_income_vulnerable_groups).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, public_welfare_bureaucracy).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_holders_high_earners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_rights_holders).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, socioeconomic_rights_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, human_dignity_principle).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, minimum_core_obligations_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, finance ministries, and constitutional bodies levy the taxes, write the eligibility rules, and operate the programs that deliver healthcare, income support, and housing. They are bound by their own entrenched commitments: unwinding the obligation requires supermajorities or formal amendment that has rarely been attempted. Their horizon spans generations of budget cycles.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_welfare_state, agenda_setter,
    institutional, generational, constrained, national).

% Receive the material floor - treatment, transfers, shelter - that the obligation secures. Individually they hold little leverage over program design and cannot purchase equivalent protection privately; collectively they form an electoral constituency that successive governments court. Leaving the system means forfeiting subsistence-level security.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, low_income_vulnerable_groups, beneficiary,
    powerless, immediate, trapped, national).

% Supply the funding through progressive taxation on income, property, and estates. Some capital relocates to lower-tax jurisdictions, but citizenship, family, and regulatory ties keep most in place; their principal levers are lobbying, litigation, and anti-expansion politics. They bear the marginal cost of every widening of the guarantee.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_holders_high_earners, payer,
    powerful, biographical, constrained, global).

% Carry the dignity-based limits on speech that travel with the security guarantee in several jurisdictions - restrictions on incitement and demeaning expression justified as protecting the security of vulnerable persons. Speech cannot be relocated across borders the way capital can, so their recourse is litigation and jurisprudential argument.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_rights_holders, payer,
    moderate, biographical, constrained, national).

% Administers the programs day to day: casework, disbursement, compliance auditing. Agencies retain administrative budgets scaled to program size, and professional careers form inside the institutions. Over decades the organizations have fused with their functions - restructuring them threatens the working identities of the people who run them.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, public_welfare_bureaucracy, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, public_welfare_bureaucracy, agenda_setter).

% Mutual-aid societies, fraternal orders, charitable hospitals, and private insurers operated much of the pre-welfare provision landscape. State provision crowded many out or absorbed them into complementary roles; they are rarely seated when entitlement scope is debated, though they hold operational knowledge of alternative delivery.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, private_provision_providers, excluded,
    organized, biographical, mobile, national).

% Treaty committees and regional courts review state performance against the obligation, elaborate minimum-core content in general comments, and document retrogression during austerity periods. They command no taxing power; their instruments are interpretation and reputational supervision.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, treaty_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, low_income_vulnerable_groups).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools societal risk and guarantees a material floor - healthcare, income support, housing - solving adverse-selection and free-rider failures that leave markets and voluntary charity unable to secure baseline life conditions for those who cannot pay.
% TRANSFER_FUNCTION: Moves tax revenue from property holders and higher earners to low-income households via transfers and in-kind services; where dignity-based expression limits bundle with the security guarantee, it also moves expressive latitude from speakers toward protected groups' dignity claims.
% ABSENT_VOICES: Private provision providers (mutual aid, fraternal societies, private insurers) crowded out by state provision; libertarian constitutional scholars who read the same text as a limit on state action rather than a spending mandate; future taxpayers who will service the accumulated obligations - none of these are seated in the drafting or review fora that produced the entitlement canon.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse healthcare and income floors for tens of millions, strand professions and budget lines built around the programs, and force emergency reorganization of household finances, labor markets, and fiscal policy - the arrangements of every named seat depend on the obligation continuing.
% FOUNDING_PROBLEM: Mass industrial-era destitution: market volatility left large populations without bare life security, and the Depression plus the Second World War made unconditional reliance on markets or charity untenable. The drafters sought a guarantee that persons would not perish from want.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by economic-historical scholarship documenting pre-welfare-state destitution and mortality series, ILO and OECD historical social-expenditure reconstructions, and legal histories of the New Deal and postwar settlements written by non-beneficiary academics. Recipient advocacy alone attests nothing here; the historical record does.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: the transfer is compulsory with identifiable payers, but by this reading's lights much of it is owed-fulfillment; the bundled speech-limit vector adds extraction with a weaker offsetting benefit even on this reading's own accounting. Suppression 0.50 is a raw structural property, unscaled - the engine scales only extractiveness (by directionality and scope): taxation is legally compelled, private alternatives are crowded out rather than banned, beneficiary exit is structurally impossible, and payer exit is constrained by citizenship ties. Theater 0.32: delivery is real, but aspirational clauses lacking enforceable minimum content, periodic state-reporting rituals, and symbolic reaffirmations contribute a growing performative share. Accessibility_collapse 0.45: alternatives persist at the margins (private provision survives, jurisdictions differ, electorates can reverse course). Resistance 0.62: sustained anti-expansion politics, property-rights litigation, and speech-rights pushback meet the obligation continuously. All three tracked series share one six-point grid (t=0..75); suppression_requirement rises through the mid-century enforcement build-out, then plateaus as compliance normalized - the flat tail reflects a stable enforcement picture, not a second dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat the arrangement is the fulfillment of a duty owed to them; from the payer seats the same statutes are compelled transfer and censored expression; from the agenda-setter seat it is administration of settled commitments. The engine computes per-seat classifications from power, exit options, and declared position - the divergence between seats is the datum the corpus exists to take, not something the authored claim adjudicates. The claim (tangled_rope) and the metrics are independent authored facts: a reader who thinks this arrangement is benign coordination or pure predation is invited to check the computed per-seat verdicts against this file's structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for low_income_vulnerable_groups (trapped exit pins them near the full-subsidy end) and for public_welfare_bureaucracy (nominal beneficiary; its budget retention is a capture-flavored wrinkle the derivation reads as a mild beneficiary position rather than a captured-administrator spike). Victim declarations drive high d for property_holders_high_earners and expression_rights_holders; the latter sit nearer the full-target end because speech cannot arbitrage across borders the way capital can. National scope with globally mobile capital among payers moderates effective extraction slightly relative to a closed economy. No directionality_overrides are authored: the derivation from declared positions, power atoms, and exit options reproduces the intended seat relationships, so overrides would add nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - mass destitution under market volatility - has been transformed rather than solved: acute want receded dramatically, but persistent relative poverty, housing stress, and pandemic-scale shocks keep versions of the problem live, hence founding_problem_status 'contested' rather than 'dead'. Claiming tangled_rope prevents twin mislabels: a pure-rope reading would erase the real asymmetric burden on property and expression rights; a pure-snare reading would erase the genuine coordination achievement (risk pooling no voluntary scheme replicates at population scale). Because the mandate still tracks a live-if-transformed problem, no mandatrophy resolution is declared; the mismatch consumer should watch the status-x-verdict cell for zombie drift as demographic aging strains funding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel udhr_article_3 (reading_id: positive_entitlement_reading). What would change structurally if a sibling reading were adopted instead?',
    'Comparative authorship: the negative-liberty sibling authors high epsilon over the identical referent with the victim set redrawn as anyone subjected to state action; the procedural hybrid narrows the party set to due-process claimants. Adoption of either sibling replaces this file''s beneficiary/victim structure wholesale.',
    'Under the negative-liberty sibling this arrangement computes as high-extraction imposition; under the hybrid the material-provision question is left unclassified by design. The disagreement is located in the meaning of ''security of person'', not in any measurable quantity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three readings of the Article 3 kernel; sibling adoptions replace the structural data.').

omega_variable(
    bundled_expression_component_separability,
    'Does the dignity-based speech-restriction vector belong to this constraint, or is it a separate constraint riding on the security guarantee?',
    'Author a standalone story for the speech-restriction regime and test whether this file''s epsilon stabilizes near 0.48 with victims reduced to property holders; instability across the split indicates genuine coupling.',
    'If separable, this constraint''s epsilon falls and its victim set shrinks; if inseparable, part of the measured extraction is intrinsic to how this reading operationalizes security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundled_expression_component_separability, conceptual, 'Whether bundled expression limits are intrinsic to the material-provision obligation or a detachable rider.').

omega_variable(
    fiscal_sustainability_of_open_ended_obligation,
    'Can open-ended material obligations remain funded through demographic aging and growth slowdown?',
    'Long-run fiscal projections cross-checked against realized social-expenditure trajectories in aged societies (Japan, Southern Europe).',
    'If unsustainable, the obligation converts toward an unfunded mandate - theater rises while retrenchment conflict drives resistance and suppression_requirement upward together.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_of_open_ended_obligation, empirical, 'Long-run fundability of the guarantee under demographic strain.').

omega_variable(
    provision_dependency_empowerment_balance,
    'Does guaranteed provision strengthen the security it promises, or does unconditional delivery erode workforce attachment and self-provision enough to offset the floor?',
    'Natural experiments across generosity and conditionality reforms (negative-income-trial evidence, childcare expansions, sanction regimes) with long-run labor and health outcomes.',
    'If dependency effects dominate, the coordination-function credit shrinks and the extraction assessment worsens at every seat; if empowerment dominates, effective extraction damps further below the authored base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provision_dependency_empowerment_balance, empirical, 'Net effect of unconditional provision on the security it guarantees.').

omega_variable(
    minimum_core_content_ambiguity,
    'What is the irreducible minimum content of the obligation - how much healthcare, income, and housing does fulfillment require?',
    'Convergence (or its absence) across treaty-body general comments, comparative constitutional enforcement (Grootboom-style judgments), and indicator-based monitoring frameworks.',
    'Wide ambiguity inflates theater_ratio (commitment without enforceable content); convergence would let enforcement concentrate and theater fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_core_content_ambiguity, conceptual, 'Enforceable-floor ambiguity driving performative compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t45, udhr_article_3__positive_entitlement_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t60, udhr_article_3__positive_entitlement_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_tr_t75, udhr_article_3__positive_entitlement_reading, theater_ratio, 75, 0.32).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_a3_pos_ent_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_be_t45, udhr_article_3__positive_entitlement_reading, base_extractiveness, 45, 0.53).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_be_t60, udhr_article_3__positive_entitlement_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_be_t75, udhr_article_3__positive_entitlement_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_a3_pos_ent_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t0, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t15, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t30, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_su_t45, udhr_article_3__positive_entitlement_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t45, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_su_t60, udhr_article_3__positive_entitlement_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t60, observed).
narrative_ontology:measurement(udhr_a3_pos_ent_su_t75, udhr_article_3__positive_entitlement_reading, suppression_requirement, 75, 0.5).
narrative_ontology:measurement_basis(udhr_a3_pos_ent_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: one colloquial label ('Article 3') covers three structurally distinct claims with distinct epsilon, victim sets, and failure modes. This story authors the positive-entitlement instantiation (epsilon 0.58 by its own lights); the negative-liberty sibling authors high epsilon over the identical referent; the procedural hybrid authors low epsilon with a narrower party set. Edges run to both siblings for contamination propagation: doctrinal victories in one reading shift the interpretive environment of the others without merging them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
