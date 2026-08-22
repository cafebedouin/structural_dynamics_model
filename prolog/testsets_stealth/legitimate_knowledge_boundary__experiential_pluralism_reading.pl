% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Boundary — Lived Experience and Community Validation Regime
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story authors the experiential-pluralism regime as an operating
 *   arrangement: a boundary rule under which legitimate knowledge arises from
 *   lived experience and is validated by community recognition, with
 *   methodological standards admitted as one tool among many rather than as
 *   the arbiter. The arrangement operates in patient-led and survivor-led
 *   research, community-based participatory research, indigenous and local
 *   knowledge governance, and disability advocacy epistemologies. Its
 *   structural signature is low barriers to entry, distributed validation,
 *   equal-or-higher weighting of experiential claims, and expertise treated
 *   as context-specific rather than general. The epsilon referent is the
 *   standing experiential-pluralism arrangement itself, assessed by the
 *   reading's own lights — the reading's endorsed alternative is not the
 *   referent, and no rival regime's objections are imported as established
 *   fact. The claimed_type (rope) and the metrics were authored
 *   independently: the claim states what I believe is structurally true of
 *   this arrangement; the metrics state what I believe is descriptively true
 *   of its actual operation, including costs the reading's own adherents
 *   acknowledge and manage.
 *
 * KEY AGENTS:
 *   - experiential_knowledge_holders: Primary beneficiary (moderate/identity_locked) — patient, survivor, indigenous, and disability communities whose lived experience is the arrangement's operating currency
 *   - community_validation_networks: Agenda setter (organized/constrained) — runs validation day to day, decides which accounts achieve standing, accumulates convening authority as communities mature
 *   - credentialed_experts: Dual-positioned payer/beneficiary (powerful/mobile) — credentials buy no automatic precedence inside the arrangement; methods remain admissible as one tool among many; exit to expert-arbitrated venues stays open
 *   - dissenting_community_members: Marginal payer (powerless/trapped) — bears the conformity costs of communal consensus; the arrangement's principal internal casualty class
 *   - institutional_science_bodies: Excluded arbiter (institutional/trapped) — would adjudicate if invited; the boundary rule assigns them no chair
 *   - participatory_program_funders: Incidental beneficiary (institutional/mobile) — purchases trusted access to populations that decline institutional contact
 *   - sts_analysts: Analytical observer (analytical/analytical) — documents successes and failure modes; collects nothing, pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.34).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.32).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Boundary — Lived Experience and Community Validation Regime").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '6ee13305-c0bf-4579-8ee9-8a30f84c789d').
narrative_ontology:cs_kernel_codification('6ee13305-c0bf-4579-8ee9-8a30f84c789d', distributed).
narrative_ontology:cs_authority_grounding('6ee13305-c0bf-4579-8ee9-8a30f84c789d', practice).
narrative_ontology:cs_interpretation_layer_present('6ee13305-c0bf-4579-8ee9-8a30f84c789d').
narrative_ontology:cs_reading_relation('6ee13305-c0bf-4579-8ee9-8a30f84c789d', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ee13305-c0bf-4579-8ee9-8a30f84c789d', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('6ee13305-c0bf-4579-8ee9-8a30f84c789d', foundational, lived_experience_confers_epistemic_standing).
narrative_ontology:cs_axiom_status(lived_experience_confers_epistemic_standing, holdable).
narrative_ontology:cs_axiom_grounding('6ee13305-c0bf-4579-8ee9-8a30f84c789d', lived_experience_confers_epistemic_standing, deontological).
narrative_ontology:cs_axiom('6ee13305-c0bf-4579-8ee9-8a30f84c789d', foundational, community_validation_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(community_validation_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6ee13305-c0bf-4579-8ee9-8a30f84c789d', community_validation_suffices_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('6ee13305-c0bf-4579-8ee9-8a30f84c789d', secondary, expertise_is_context_specific_not_general).
narrative_ontology:cs_axiom_status(expertise_is_context_specific_not_general, holdable).
narrative_ontology:cs_axiom_grounding('6ee13305-c0bf-4579-8ee9-8a30f84c789d', expertise_is_context_specific_not_general, empirically_contingent).
narrative_ontology:cs_reference_frame('6ee13305-c0bf-4579-8ee9-8a30f84c789d', experiential_parity_regime).
narrative_ontology:cs_drift_state('6ee13305-c0bf-4579-8ee9-8a30f84c789d', contemporary_institutionalization_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ee13305-c0bf-4579-8ee9-8a30f84c789d', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_program_funders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, dissenting_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_principle).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People whose authority rests on having lived what they describe — patients documenting illness courses, survivors mapping harm, residents tracking local environmental change, practitioners of inherited local knowledge. They offer testimony, recognize and vouch for one another's accounts, and gain standing to define problems and evaluate remedies without acquiring credentials. Stepping away from the community would mean stepping away from the account of their own lives that their standing rests on.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders, beneficiary,
    moderate, biographical, identity_locked, regional).

% Facilitator circles, patient-organization boards, and community research partnerships that run the validation process day to day: convening testimony sessions, drafting consensus statements, deciding which accounts achieve recognized standing and which await corroboration. As communities mature, these bodies accumulate convening power, reputational standing, and discretion over who speaks for the community.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_networks, agenda_setter,
    organized, generational, constrained, regional).

% Clinicians, scientists, and professional researchers whose certification commands deference in hospitals, universities, and agencies. Inside this arrangement their credentials purchase no automatic precedence: their input counts as one contribution among many, weighed alongside first-person accounts, and years of specialized training yield less standing than the same training yields in expert-arbitrated venues. They remain mobile — they can take their questions to journals and agencies, and their methods stay admissible as one approach among several.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, beneficiary).

% Members whose experience diverges from the community's dominant account — atypical disease trajectories, unexpected recoveries, unpopular readings of shared events. The same recognition process that amplifies conforming testimony discounts theirs, and repeated divergence marks them as unreliable narrators. Ties of friendship, shared identity, and hard-won belonging make departure costly: leaving means losing both the community and the standing it alone conferred.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, dissenting_community_members, payer,
    powerless, biographical, trapped, local).

% National academies, professional societies, and regulatory agencies whose arbitration the arrangement declines to request. They maintain views on evidentiary standards and would adjudicate disputes if invited; the boundary rule assigns them no chair, and their outputs — journal articles, guidelines, approvals — carry no automatic weight inside the community. Their formal channels remain open to them, but the community's internal standing economy does not price them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional_science_bodies, excluded,
    institutional, generational, trapped, global).

% Public-health agencies and foundations financing community-based participatory research. They gain access to populations that decline institutional contact, and a credibility that conventional studies struggle to earn; they absorb slower timelines and relationship-dependent schedules in exchange. They can redirect money toward conventional designs if community processes stop producing usable results.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_program_funders, beneficiary,
    institutional, biographical, mobile, national).

% Scholars of situated knowledge, testimony, and epistemic injustice who study the arrangement: documenting where community validation caught what institutions missed, where it hardened into orthodoxy, and articulating the standards its defenders and critics argue with. They collect nothing from its operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, sts_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_networks).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools and validates knowledge that is distributed across lived experience — illness courses, local environments, structural harms — providing a low-cost recognition channel that requires no expensive credentialing infrastructure, and keeping first-person data in play while slower methodological confirmation catches up.
% TRANSFER_FUNCTION: Moves epistemic standing and attention toward experiential knowers and community validators, and away from credentialed intermediaries; moves trust and deference from institutional review bodies to community recognition processes.
% ABSENT_VOICES: Dissenting members whose experience diverges from communal consensus are present in body but discounted in voice; credentialed specialists hold views the process never solicits as arbitration; future members inherit communally validated claims they had no hand in testing.
% DISAPPEARANCE_RATIONALE: Communities relying on the boundary rule would lose their only functioning validation channel overnight: participatory research programs would collapse back to expert mediation, standing already conferred on experiential knowers would evaporate, and domains such as contested illness and local ecology would lose the recognition infrastructure their current knowledge base depends on.
% FOUNDING_PROBLEM: Credential-gated epistemic authority systematically discounted testimony from those without institutional access — patients told their illness experience was not data, communities whose environmental observations carried no official weight — leaving whole classes of knowledge unrecognized and their holders disqualified from defining their own problems.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: philosophy of science and STS literature documenting testimonial injustice, documented histories of delayed recognition of contested conditions, and — decisively — the testimony of critics who accept that the exclusion problem was real while disputing this remedy. Institutional science bodies have themselves conceded past dismissal in reconciliation and apology statements.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).
:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.34: the arrangement is predominantly a subsidy to previously excluded knowers, but it carries real costs — dissenting members bear consensus-conformity discounts on their testimony, and credentialed experts bear a discount on invested training capital. Suppression is 0.32: the methodological route remains formally open ('one tool among many'), so alternatives do not collapse, but social pressure against importing outside authority to trump communal consensus is real and grows as communities institutionalize. Theater_ratio is 0.28: most validation activity is functional, but institutionalized participatory programs increasingly stage consultation rituals whose outputs bind nothing. Accessibility_collapse is 0.30 — low, as befits an arrangement that leaves the methodological alternative fully available; resistance is 0.62, reflecting sustained institutional pushback from the credentialed-expertise establishment. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. The temporal series runs on one shared grid (t=0..30 at step 6) across all three tracked metrics: extraction, theater, and enforcement intensity all drift gently upward as successful communities develop internal hierarchies of 'authentic experience' and facilitator bodies acquire gatekeeping discretion. The drift is monotone rather than cyclical — no oscillation mechanism is posited. Despite the declared victim class, I claim rope: the dissenting-member cost is a boundary failure mode the arrangement's own corrective norms target, not a load-bearing transfer channel; whether that holds is exactly what the consensus_truth_tracking and internal_dissent_suppression_mechanism omegas are staged to resolve, and divergence between my claim and the engine's per-seat computations is the datum the corpus exists to collect.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the experiential_knowledge_holder seat the arrangement is a subsidy: standing granted without credentialing, identity-fused with the community that grants it. From the dissenting_community_member seat the same process operates as extraction: trapped, powerless, testimony discounted by the very mechanism that empowers neighbors. From the credentialed_expert seat the arrangement is a mild target position heavily damped by mobility — the authority discount stings, but exit to expert-arbitrated venues is cheap and methods remain admissible. From the funder seat it is near-symmetric: credibility gained, timelines lost. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (experiential_knowledge_holders, participatory_program_funders) derive low directionality — the arrangement subsidizes them, and the holders' identity_lock stabilizes rather than penalizes their position. The declared victim (dissenting_community_members) derives high directionality amplified by trapped exit and powerless standing — they sit nearest the full-target end of any seat in the story. Credentialed_experts carry a directionality_override at 0.55: the automatic derivation would read their secondary beneficiary role plus mobile exit and place them near the beneficiary end, but descriptively they bear a real authority discount on sunk training capital — mid-range, target-leaning. The override is keyed to the 'powerful' atom, which in this story names only that seat. Community_validation_networks derive low directionality (the arrangement confers their authority), yet they are the receipt seat for its gains — receipt-of-gain and directionality are distinct facts, and the gain_flow field records the former, not the latter. Institutional_science_bodies are excluded rather than coordinated: their exclusion is a structural feature of the boundary rule, not an extraction channel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credential-gated exclusion of experiential testimony — is live, so no mandatrophy is declared and the R5 mismatch consumer should find status=live paired with verdict=world_rearranges, yielding no zombie flag. The classification discipline cuts both ways here: reading the arrangement as pure extraction would erase the genuine inclusion gains that constitute its coordination function; reading it as frictionless coordination would erase the dissenting-member costs its temporal series shows accumulating. The theater_ratio series is the early-warning instrument: if performative consultation continues replacing binding validation as programs institutionalize, the arrangement drifts toward maintained performance rather than function, and the rope claim should be revisited against the computed type rather than defended by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel — what would the sibling readings change structurally if instantiated instead?',
    'Comparative classification across the three reading files in the family; engine-computed foreclosure from axiom contradictions and drift states rather than authored assertion.',
    'The credentialed_expertise_reading reverses the beneficiary structure entirely (experts subsidized, experiential knowers targeted, high barriers); the hybrid_coproduction_reading redistributes authority rather than relocating it. Epsilon, directionality, and computed type are re-derived per reading; no value from this file transfers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this story instantiates the experiential_pluralism_reading; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    consensus_truth_tracking,
    'Does distributed community validation track truth over time, or does it track narrative coherence and social solidarity?',
    'Longitudinal audit of communally validated claims against subsequent methodological confirmation, with survival analysis of validated claims by domain.',
    'Poor truth-tracking would raise effective extractiveness (deference collected without reliability delivered) and push the computed type toward tangled_rope; strong tracking would confirm the coordination-first reading and hold epsilon near its current value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_truth_tracking, empirical, 'Reliability of community validation as an epistemic filter.').

omega_variable(
    internal_dissent_suppression_mechanism,
    'Is the discounting of divergent testimony inside communities structurally enforced (social sanction, standing withdrawal) or internalized (divergents absorb the discount as self-doubt)?',
    'Post-departure testimony trajectories of former members: if self-discounting persists after exit from the community, the internalized component is substantial.',
    'An internalized component means effective suppression exceeds the structural measure — dissenters carry the mechanism with them after exit, and the victim-class costs are larger than observable sanction data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_dissent_suppression_mechanism, empirical, 'Structural versus internalized suppression of intra-community dissent.').

omega_variable(
    parity_domain_scope,
    'Does experiential parity extend to all knowledge domains, or only to domains reachable from lived experience?',
    'Survey of which disputes communities actually submit to parity arbitration versus which they route to specialist input; comparison across the reading''s own application practice.',
    'Universal parity demotes load-bearing specialist knowledge (dosage, structural engineering, epidemiology) and raises epsilon materially; domain-bounded parity keeps epsilon near the authored value and preserves the coordination-first classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parity_domain_scope, conceptual, 'Scope ambiguity in the parity claim — the largest lever on this reading''s extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exp_plur_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(exp_plur_tr_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(exp_plur_tr_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(exp_plur_tr_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(exp_plur_tr_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(exp_plur_tr_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(exp_plur_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(exp_plur_be_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(exp_plur_be_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(exp_plur_be_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(exp_plur_be_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(exp_plur_be_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 30, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(exp_plur_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(exp_plur_su_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 6, 0.19).
narrative_ontology:measurement(exp_plur_su_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(exp_plur_su_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 18, 0.25).
narrative_ontology:measurement(exp_plur_su_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(exp_plur_su_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 30, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'whose knowledge counts' covers three structurally distinct legitimacy regimes and is decomposed per the epsilon-invariance principle into three stories sharing the legitimate_knowledge_boundary kernel. This file instantiates the experiential pluralism regime (experiential knowers subsidized, credentialed authority discounted, low barriers). The credentialed_expertise_reading instantiates the inverse beneficiary structure (experts subsidized, uncredentialed testimony discounted). The hybrid_coproduction_reading instantiates an integrative regime that redistributes rather than relocates authority. The credentialed regime is the historical baseline this reading defines itself against; this reading supplies the experiential-validity component the hybrid integrates. Each story carries its own epsilon, beneficiaries, and classification; they are linked here and in their own files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
