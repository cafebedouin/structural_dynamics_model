% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Principled Intervention Reading of Constitutional Secularism
 *   domain: constitutional/political/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_secularism
 *   kernel: the principled_intervention_reading, under which the state may
 *   enter religious affairs when doing so advances social reform or protects
 *   weaker sections within religious communities. Per the epsilon-invariance
 *   discipline, this file authors only this reading: the
 *   strict_neutrality_reading (equal distance, no interference) and the
 *   reformist_reading (affirmative duty superseding autonomy claims) are
 *   separate constraints in separate files, linked through
 *   network.affects_constraints. The structural delta this reading carries is
 *   threefold: differential treatment of religious communities becomes
 *   legitimate when reform objectives justify it; state authority expands
 *   into a domain otherwise governed by community self-rule; and the reform
 *   agenda becomes capturable by majority preferences about which communities
 *   need reforming. The epsilon referent is the standing intervention regime
 *   itself, assessed by this reading's own lights — which endorse
 *   intervention's protective purpose while registering its expanding
 *   authority footprint and capture exposure.
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setting adjudicator and authority recipient (institutional/analytical) — controls the essential-versus-reformable line
 *   - national_legislature: agenda-setting enactor (institutional/constrained) — writes the reform statutes
 *   - weaker_sections_within_communities: primary intended beneficiary (organized/identity_locked) — bears exclusion, relies on external enforcement
 *   - social_reform_movements: secondary beneficiary (organized/mobile) — supplies the agenda, collects standing and victories
 *   - religious_institution_heads: primary payer (institutional/constrained) — loses administrative and doctrinal control
 *   - dominant_community_elites: payer (powerful/constrained) — loses ritual gatekeeping, retains wealth and political leverage
 *   - minority_religious_communities: payer under capture exposure (moderate/constrained) — conditionally protected, differentially targeted
 *   - ordinary_believers: dual-positioned (moderate/identity_locked) — gain access, lose self-governance
 *   - unrepresented_traditional_practitioners: excluded seat (powerless/trapped) — their practices are decided without them
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — documents differential application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.66).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Principled Intervention Reading of Constitutional Secularism").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional/political/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '8f480776-5560-47b8-88ad-f26c1dd933b6').
narrative_ontology:cs_kernel_codification('8f480776-5560-47b8-88ad-f26c1dd933b6', fixed_text).
narrative_ontology:cs_authority_grounding('8f480776-5560-47b8-88ad-f26c1dd933b6', lineage).
narrative_ontology:cs_interpretation_layer_present('8f480776-5560-47b8-88ad-f26c1dd933b6').
narrative_ontology:cs_reading_relation('8f480776-5560-47b8-88ad-f26c1dd933b6', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f480776-5560-47b8-88ad-f26c1dd933b6', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('8f480776-5560-47b8-88ad-f26c1dd933b6', foundational, reform_objectives_legitimate_differential_intervention).
narrative_ontology:cs_axiom_status(reform_objectives_legitimate_differential_intervention, holdable).
narrative_ontology:cs_axiom_grounding('8f480776-5560-47b8-88ad-f26c1dd933b6', reform_objectives_legitimate_differential_intervention, instrumental).
narrative_ontology:cs_axiom('8f480776-5560-47b8-88ad-f26c1dd933b6', foundational, weaker_section_protection_overrides_religious_autonomy).
narrative_ontology:cs_axiom_status(weaker_section_protection_overrides_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('8f480776-5560-47b8-88ad-f26c1dd933b6', weaker_section_protection_overrides_religious_autonomy, deontological).
narrative_ontology:cs_reference_frame('8f480776-5560-47b8-88ad-f26c1dd933b6', reform_permissive_secularism).
narrative_ontology:cs_drift_state('8f480776-5560-47b8-88ad-f26c1dd933b6', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f480776-5560-47b8-88ad-f26c1dd933b6', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, weaker_sections_within_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, social_reform_movements).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, national_legislature).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_institution_heads).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, dominant_community_elites).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, minority_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, ordinary_believers).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, ordinary_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which religious practices count as essential (and therefore shielded) versus reformable (and therefore open to state action), issuing rulings on temple entry, institutional administration, and personal-law questions. Each ruling extends or confirms the court's gatekeeping position over religious life, and the docket of such questions has grown steadily. The seat is constituted by this adjudicative function; stepping back from it would mean surrendering the jurisdiction the function generates.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, constitutional_courts, beneficiary).

% Enacts the reform statutes through which intervention operates: temple-entry acts, institutional takeover laws, personal-law amendments. Responds to reform coalitions pressing for action and to religious counter-mobilization pressing for restraint, and bears the electoral consequences of whichever direction it takes.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, national_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Members excluded from worship spaces, priesthood, and communal resources on hereditary or ritual grounds. Internal community mechanisms offer no remedy because those mechanisms are controlled by the groups maintaining the exclusion, and leaving the community would forfeit kinship, livelihood networks, and belonging. External legal enforcement is the one channel through which access and dignity claims can be pressed.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, weaker_sections_within_communities, beneficiary,
    organized, generational, identity_locked, national).

% Campaign organizations that supply the reform agenda through litigation, draft legislation, and public mobilization. The intervention channel gives them standing, funding, and visible victories; if the channel closed they could redirect their organizing to other domains, though accumulated case law and coalition ties would depreciate.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, social_reform_movements, beneficiary,
    organized, biographical, mobile, national).

% Trustees, pontiffs, and councils administering temples, monasteries, and church property and ritual life. Statutes place institutions under state boards or courts redefine which practices are dispensable, requiring restructuring of ritual life they regard as continuous transmission received from predecessors. Their authority is inseparable from the institution they administer; resistance runs through litigation and moral suasion, both slow.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_institution_heads, payer,
    institutional, generational, constrained, national).

% Upper-status notables whose social position rests on controlling access to sacred space and ritual hierarchy. Intervention dismantles their gatekeeping role; they retain wealth and political influence but lose the ritual dimension of their standing. They fund counter-mobilization, sponsor litigation, and press legislatures for carve-outs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, dominant_community_elites, payer,
    powerful, generational, constrained, national).

% Communities whose institutions and personal-law systems become candidate targets whenever the reform agenda turns their way. They receive some protections from the same legal framework, but bear the standing risk that what counts as reform is defined by majority preferences. Their leaderships lobby for exemption categories and autonomy carve-outs and document differential application across communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_religious_communities, payer,
    moderate, generational, constrained, national).

% Attend services, fund institutions, and inherit ritual obligations. Where exclusion previously barred them, they gain restored access; at the same time they lose a measure of communal self-governance as state boards and courts come to mediate religious life. The tradition structures family and identity, so exiting it is not a realistic response to dissatisfaction with how it is governed.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, ordinary_believers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, ordinary_believers, payer).

% Custodians of folk and local rituals, festivals, and healing traditions that reform frameworks classify as backward or dispensable. They are not parties to the litigation or consultation processes that determine whether their practices survive; their objection channels run through oral tradition and local assemblies, which carry no standing in the proceedings that decide.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, unrepresented_traditional_practitioners, excluded,
    powerless, biographical, trapped, local).

% Track how intervention doctrines evolve across jurisdictions, document differential application between communities, and compile the comparative record that courts, reformers, and autonomy defenders all cite in support of their positions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a protection problem that intra-community mechanisms cannot solve: members oppressed within their own religious community lack the internal power to reform it and cannot exit it without forfeiting kinship, livelihood, and belonging, so an external enforcement point is the only available guarantor of minimum access and dignity standards inside the community.
% TRANSFER_FUNCTION: Moves regulatory authority over religious practice from community self-governance to state institutions; moves access and status goods (entry to worship spaces, eligibility for priesthood and office, protection from ritual exclusion) toward weaker sections; moves compliance and restructuring costs onto religious institutions and dominant elites.
% ABSENT_VOICES: Practitioners of folk and local traditions slated for classification as dispensable are never consulted; minority community leaderships are heard reactively, after reform agendas turn toward them, rather than seated when agendas are set; internal dissenters whose preferred remedy differs from the entry-rights framing (for example, wanting material redistribution rather than ritual access) have no channel in a process organized around litigation.
% DISAPPEARANCE_RATIONALE: If the intervention power vanished overnight, temple-entry settlements, personal-law reform structures, and institutional-administration arrangements would unwind; communities would revert to internal governance with the exclusionary outcomes that originally prompted intervention; weaker sections would lose their only external enforcement point; and the courts and legislatures would surrender a body of jurisdiction and statute they currently administer.
% FOUNDING_PROBLEM: Constitution-builders inherited religiously sanctioned hierarchies — hereditary exclusion from worship, ritualized status subordination, gender-barred access — that internal reform movements had failed to dismantle at scale, and had to decide how a democratic state could dissolve these without either abandoning the people they oppressed or abolishing religious freedom altogether.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is attested from outside the benefiting parties: continuing litigation over ritual exclusion initiated by affected worshippers, human-rights commission reports documenting caste- and gender-based denial of access, and sociological survey data on ritual exclusion — sources with no stake in the intervention power's continuation.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type and metrics are authored independently. The claim is tangled_rope because the structure holds BOTH a genuine coordination function (external protection for members who cannot reform or exit their communities) AND asymmetric extraction (authority and compliance costs flow upward and outward from religious institutions to state seats), held together by active enforcement — courts policing compliance, statutes compelling restructuring. Extractiveness is 0.62: substantial but bounded, because this reading's own lights endorse much of what the regime extracts as the price of protection; the residual is authority aggrandizement and selectively applied burden. Suppression is 0.66 and reflects legal compulsion — statutory override of autonomy claims, contempt and compliance machinery — not suppression of exit; it is authored as a raw structural property and is not scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine's computation). Theater_ratio 0.38: core enforcement is real, but a growing share of activity is committee reports, commissioned studies, and symbolic reform announcements that substitute for structural change. Accessibility_collapse 0.42: alternatives persist — strict-neutrality regimes operate elsewhere, internal reform remains conceivable, community self-governance is a live counter-model — so understanding this constraint does not close the option space. Resistance 0.58: sustained institutional litigation, elite-funded counter-mobilization, and minority autonomy campaigns meet the regime continuously. The temporal series run on one shared seven-point grid (0, 12, 24, 36, 48, 60, 72) with all three tracked metrics authored at every point; trajectories show extraction accumulating, enforcement machinery hardening, and performative share rising together over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setting seats (courts, legislature) the arrangement is legitimate constitutional craft: a carefully bounded power exercised for protective ends. From the weaker-section seat it is the only available protection — the alternative is not neutrality but abandonment. From the religious-institution and dominant-elite seats it is dispossession of transmitted authority under a reform label others wrote. From the minority-community seat it is conditional safety: protection today, candidate-target status whenever the agenda turns. Same-level differentiation is sharpest between minority communities and dominant elites: both resist, but the elites retain wealth and political leverage to shape the agenda, while minority communities mostly defend — which is why identical nominal positions produce different effective burdens. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Weaker sections and reform movements sit near the beneficiary end (low d): the regime subsidizes their access claims and they pay little of its cost. Courts and legislature are declared beneficiaries because they collect the regime's principal product — jurisdiction and statute — while paying no compliance cost; this declaration is deliberate, since the structural delta of this reading is precisely state-authority expansion, and without it the derivation would underweight the authority accrual. Religious institution heads, dominant elites, and minority communities sit near the target end (high d): they bear restructuring costs, lost control, and differential targeting respectively, with constrained exit keeping them near full-target. Ordinary believers are dual-declared and land mid-range: genuine access gains against real self-governance losses, with identity-locked exit preventing escape from either side of the ledger. No directionality_overrides are authored: the derivation from declarations plus exit options already tracks the true relationships, and the override mechanism keys on power atoms, which would misfire here because institutional-power seats (courts, legislature, institution heads) occupy opposite directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: exclusionary practices persist and new fronts (gender-exclusion disputes, folk-tradition classifications) keep arriving, so no mandatrophy resolution is declared and the status-by-verdict pair (live, world_rearranges) raises no obsolescence flag. The classification nonetheless prevents two mislabels. Reading the arrangement as pure rope would erase the extraction asymmetry — the authority accrual to state seats and the selective burden across communities that the temporal series shows accumulating. Reading it as a snare would erase the protection function that weaker sections cannot replace from any other seat: their exit is identity-locked and their internal remedy channels are controlled by the very groups the regime restrains. Tangled rope holds both facts: coordination that works, extraction that grows, enforcement that keeps the pair fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the principled_intervention_reading of the constitutional_secularism kernel — would the strict_neutrality_reading or the reformist_reading, instantiated over the same constitutional text, produce a structurally different constraint?',
    'Comparative analysis of jurisdictions and case lines where each sibling reading governs: measure epsilon, victim sets, and enforcement profiles under each reading and verify they diverge as the structural deltas predict.',
    'Under strict_neutrality the same state acts register as autonomy violations with near-zero legitimate coordination content, shifting victims toward all intervened institutions; under reformist the regime''s timidity becomes the defect, shifting victims toward the unreformed oppressive practices it leaves standing. Classification of this file''s arrangement is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    majoritarian_capture_gradient,
    'Does the reform agenda track the distribution of vulnerability or the distribution of majority power — is differential application across religious communities explained by documented reform need or by which community the majority prefers to reform?',
    'Cross-community comparison of intervention rates against independently documented exclusion and harm rates, over time: convergence indicates need-driven application; divergence indicates capture.',
    'Confirmed capture would push the arrangement toward snare-flavored operation with minority communities as wholesale victims and the coordination function reduced to cover; refuted capture would stabilize the tangled_rope reading with extraction attributable to authority accrual rather than targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_gradient, empirical, 'Whether differential treatment follows need or majority preference.').

omega_variable(
    essential_practices_boundary_stability,
    'Where does the operative line between protected religious core and reformable practice sit, and is it drawn consistently across communities and over time?',
    'Longitudinal doctrinal analysis of holdings classifying practices as essential versus dispensable, testing consistency across communities and stability across decades.',
    'An unstable or asymmetrically drawn line degrades the coordination function into selective authorization — the same act is protected in one community and reformable in another — which converts the regime''s legitimacy premise into a targeting instrument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_practices_boundary_stability, conceptual, 'Stability and even-handedness of the essential-versus-reformable boundary.').

omega_variable(
    authority_persistence_vs_problem_persistence,
    'Does the intervention power persist because exclusionary problems persist, or because the seats receiving its authority maintain it?',
    'Compare intervention activity volume against measured exclusion prevalence across the interval: coupling indicates problem-driven persistence; decoupling indicates authority-driven persistence.',
    'Decoupling would signal mandatrophy onset — the arrangement surviving on inertia and theatrical maintenance — and predict drift toward piton dynamics even while the founding problem''s residue keeps victims attached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_persistence_vs_problem_persistence, empirical, 'Whether persistence tracks the problem or the authority it generates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(cons_tr_t12, constitutional_secularism__principled_intervention_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__principled_intervention_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(cons_tr_t36, constitutional_secularism__principled_intervention_reading, theater_ratio, 36, 0.27).
narrative_ontology:measurement(cons_tr_t48, constitutional_secularism__principled_intervention_reading, theater_ratio, 48, 0.31).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__principled_intervention_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(cons_tr_t72, constitutional_secularism__principled_intervention_reading, theater_ratio, 72, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(cons_be_t12, constitutional_secularism__principled_intervention_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__principled_intervention_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(cons_be_t36, constitutional_secularism__principled_intervention_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement(cons_be_t48, constitutional_secularism__principled_intervention_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__principled_intervention_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(cons_be_t72, constitutional_secularism__principled_intervention_reading, base_extractiveness, 72, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t12, constitutional_secularism__principled_intervention_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__principled_intervention_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(cons_su_t36, constitutional_secularism__principled_intervention_reading, suppression_requirement, 36, 0.57).
narrative_ontology:measurement(cons_su_t48, constitutional_secularism__principled_intervention_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__principled_intervention_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(cons_su_t72, constitutional_secularism__principled_intervention_reading, suppression_requirement, 72, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional secularism' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — strict_neutrality_reading (baseline framing, minimal intervention legitimacy), principled_intervention_reading (this file: intervention permitted under reform justification), and reformist_reading (affirmative duty superseding autonomy). Epsilon differs across the family because the referent is assessed by each reading's own lights over the same standing arrangement. Influence runs downstream: the strict-neutrality framing supplies the textual foundation this reading departs from, and this reading builds the doctrinal infrastructure (essential-practices gatekeeping, reform precedent) on which the reformist reading either constructs its duty-claims or collides against its limits. All family members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
