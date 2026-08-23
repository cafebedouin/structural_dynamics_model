% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Messianic Deferral Regime for the Sacrificial Commandments (Kodashim Study-as-Readiness)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   A dominant strand of post-Temple halakhic life holds that the
 *   commandments of sacrifice (kodashim) remain binding in principle but
 *   cannot be performed until the Temple stands again; until then, the
 *   community's obligation is discharged as readiness — systematic study that
 *   keeps the sacrificial order transmissible, so that restoration finds the
 *   capacity intact rather than requiring reconstruction. This story
 *   instantiates the messianic_deferral reading of that kernel: the
 *   commandment is temporally suspended but not obsolete, and study is
 *   justified by future contingency rather than present fulfillment. The
 *   arrangement coordinates genuinely (an unbroken transmission chain across
 *   roughly two millennia of non-performance) while transferring real present
 *   costs — formative decades of elite scholarly capacity, communal
 *   attention, philanthropic priority — toward a payoff owned by a cohort
 *   that does not yet exist. CONSTRAINT-FAMILY NOTE (epsilon-invariance
 *   decomposition): the colloquial label 'status of the sacrificial
 *   commandments' covers three structurally distinct claims, written as
 *   separate stories linked by network.affects_constraints. This reading
 *   authors epsilon at 0.45 — moderate, because the reading's own lights
 *   endorse the arrangement and the transfer is opportunity cost rather than
 *   coercion. The study_as_performance sibling authors lower epsilon (study
 *   itself fulfills; the kernel is occupied now). The performance_only
 *   sibling, assessing the same curriculum as guardianship of a husk, authors
 *   materially higher epsilon relative to value delivered. Same referent
 *   arrangement, different readings, different epsilon — which is why they
 *   are three files, not one. KEY AGENTS (by structural relationship): -
 *   halakhic_scholarly_establishment: agenda-setter and principal collector
 *   (institutional/constrained) — administers the readiness curriculum,
 *   collects vocation, authority, and institutional perpetuation -
 *   talmudic_students: primary present-day target (moderate/identity_locked)
 *   — devote formative decades to laws that cannot be performed -
 *   diaspora_observant_communities: secondary beneficiary
 *   (organized/constrained) — receive identity continuity and covenantal
 *   meaning - future_restored_community: intended terminal beneficiary
 *   (powerless/trapped) — prospective cohort depending wholly on present
 *   fidelity - present_need_bearers: diffuse target (powerless/constrained) —
 *   present needs subordinated to readiness investment -
 *   temple_readiness_movement: activist beneficiary
 *   (organized/identity_locked) — operationalizes restoration preparation -
 *   reform_and_secular_descendants: excluded voice (organized/mobile) —
 *   resolved the founding problem by abrogation and left the conversation -
 *   halakhic_structural_analyst: analytical observer (analytical/analytical)
 *   — sees the full structure from outside commitment
 *
 * KEY AGENTS:
 *   - halakhic_scholarly_establishment: agenda-setter and principal collector (institutional/constrained)
 *   - talmudic_students: primary present-day target (moderate/identity_locked)
 *   - diaspora_observant_communities: secondary beneficiary (organized/constrained)
 *   - future_restored_community: intended terminal beneficiary (powerless/trapped, prospective)
 *   - present_need_bearers: diffuse target (powerless/constrained)
 *   - temple_readiness_movement: activist beneficiary (organized/identity_locked)
 *   - reform_and_secular_descendants: excluded voice (organized/mobile)
 *   - halakhic_structural_analyst: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.3).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Messianic Deferral Regime for the Sacrificial Commandments (Kodashim Study-as-Readiness)").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'a8486be0-4718-4d18-8c18-4b885302e1e7').
narrative_ontology:cs_kernel_codification('a8486be0-4718-4d18-8c18-4b885302e1e7', fixed_text).
narrative_ontology:cs_authority_grounding('a8486be0-4718-4d18-8c18-4b885302e1e7', lineage).
narrative_ontology:cs_interpretation_layer_present('a8486be0-4718-4d18-8c18-4b885302e1e7').
narrative_ontology:cs_reading_relation('a8486be0-4718-4d18-8c18-4b885302e1e7', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('a8486be0-4718-4d18-8c18-4b885302e1e7', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_axiom('a8486be0-4718-4d18-8c18-4b885302e1e7', foundational, suspension_preserves_residual_obligation).
narrative_ontology:cs_axiom_status(suspension_preserves_residual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a8486be0-4718-4d18-8c18-4b885302e1e7', suspension_preserves_residual_obligation, deontological).
narrative_ontology:cs_axiom('a8486be0-4718-4d18-8c18-4b885302e1e7', foundational, study_sustains_restoration_readiness).
narrative_ontology:cs_axiom_status(study_sustains_restoration_readiness, holdable).
narrative_ontology:cs_axiom_grounding('a8486be0-4718-4d18-8c18-4b885302e1e7', study_sustains_restoration_readiness, instrumental).
narrative_ontology:cs_reference_frame('a8486be0-4718-4d18-8c18-4b885302e1e7', sacrificial_order_default_covenant_state).
narrative_ontology:cs_drift_state('a8486be0-4718-4d18-8c18-4b885302e1e7', post_destruction_interregnum, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a8486be0-4718-4d18-8c18-4b885302e1e7', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, future_restored_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, diaspora_observant_communities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, halakhic_scholarly_establishment).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, talmudic_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_need_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, temple_readiness_movement).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, residual_covenantal_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the academies, sets the study curriculum, ordains teachers, and decides how much sacrificial-law study the community owes. Collects vocation, standing, and institutional continuity from the arrangement it administers; its own legitimacy rests on an unbroken chain of teaching that the readiness curriculum keeps warm. Redirecting the curriculum away from sacrificial law would undercut the lineage it claims to embody, so its hands are more tied than its formal authority suggests.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_scholarly_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, halakhic_scholarly_establishment, beneficiary).

% Spend their formative decades mastering the orders of sacrifice — species, dissection, disqualification — that cannot be offered anywhere in the world today. Mastery brings standing, marriage prospects, and livelihood inside the study world, and little purchase outside it; leaving the track mid-career means forfeiting accumulated status and often communal belonging. Most were enrolled as adolescents by families and teachers before any independent choice was possible.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, talmudic_students, payer,
    moderate, biographical, identity_locked, global).

% Sustain the study obligation through tuition, donations, and the prestige they accord its masters. They receive in return a continuous identity: a liturgy that remembers the offerings, a calendar oriented toward a restored service, and the assurance that exile is an interlude rather than an end. Their exit — assimilation or adoption of a non-deferral account — is possible but severs them from the community's reward structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, diaspora_observant_communities, beneficiary,
    organized, generational, constrained, global).

% The anticipated cohort that will offer sacrifices again if restoration comes, inheriting whatever knowledge the present preserves. It does not yet exist and exerts no influence; every commitment made for its sake is made by others on its behalf. Its entire inheritance depends on the fidelity of generations that will never meet it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, future_restored_community, beneficiary,
    powerless, civilizational, trapped, global).

% Members of the community whose immediate needs — education, poverty relief, mental health, institutional upkeep — compete with readiness investment for the same philanthropic budgets, scholarly hours, and communal attention. They bear the opportunity cost diffusely and lack any organ that aggregates their claim; the deferral frame tells them their turn comes later, or in a different world.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_need_bearers, payer,
    powerless, immediate, constrained, global).

% Small activist groups that prepare vessels, garments, priestly lineages, and even animal candidates for a renewed service. They draw funding, volunteers, and public attention precisely because the deferral frame makes preparation feel urgent; their identity is fused with the restoration they await, and abandoning preparation would dissolve their reason for existing.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, temple_readiness_movement, beneficiary,
    organized, generational, identity_locked, national).

% Descendants of movements that resolved the founding problem by declaring the sacrificial order superseded rather than deferred. They stand outside the conversation entirely: they do not contest the curriculum, fund it, or feel its pull, and their very existence demonstrates that exit was always possible — which the deferral community registers as weakness of conviction rather than evidence of an open door.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, reform_and_secular_descendants, excluded,
    organized, biographical, mobile, continental).

% Studies the arrangement from outside commitment: maps how the readiness curriculum allocates attention, what the deferral frame promises and to whom, and how the rival accounts of the suspended commandment distribute costs differently. Bears no costs and collects no benefits; its only output is description.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_structural_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, halakhic_scholarly_establishment).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complete technical knowledge of the sacrificial order across an open-ended period of non-performance, so that restoration imposes no knowledge-reconstruction delay; simultaneously coordinates a dispersed community around a shared anticipatory curriculum and an unbroken chain of transmission.
% TRANSFER_FUNCTION: Moves present scholarly labor, curricular capacity, and communal attention from immediately consumable uses to maintenance of restoration-readiness; moves status, livelihood, and institutional authority to those who master and transmit the material; the deferred payoff — resumable sacrifice — is promised to a future cohort.
% ABSENT_VOICES: Reform and secular descendants rejected the frame and are outside the conversation; present-welfare advocates inside the community rarely contest curricular allocation formally; and the arrangement's nominal ultimate beneficiary — the future restored community — cannot speak at all: its interests are asserted on its behalf by the very institutions that claim to serve it, an asymmetry the deferral structure never has to answer for.
% DISAPPEARANCE_RATIONALE: If the readiness-maintenance arrangement vanished overnight, curricula would shed the sacrificial tractates within a generation, the transmission chain would break, and restoration capacity — should it ever be wanted — would have to be re-derived from scratch; the identity arc binding dispersed communities to the Temple service would need replacement, and the establishment would lose a pillar of its vocation and self-understanding.
% FOUNDING_PROBLEM: The destruction of the Second Temple severed the covenantal community from the sacrificial order its texts command: commandments that cannot be performed without an altar. The founding problem is how a community keeps commandments it cannot perform, and preserves the capacity to resume them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: academic historians of the post-70 CE period attest the rupture and the preservation problem it created; the Talmud's own sustained engagement with sacrificial detail presupposes it; and early modern and modern critics attested the problem was real while resolving it by abrogation rather than deferral. No corroborating source disputes that the founding problem existed — the dispute concerns only its solution.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The arrangement is claimed as scaffold: its entire justification is the transition it bridges — study exists so that restoration, whenever it comes, finds the capacity intact — and the tradition itself declares the termination condition (service resumed, readiness obsolete), though the sunset is eschatological rather than schedulable (see the unscheduled_sunset_validity omega). Extractiveness is moderate (0.45 at interval end): the transfer is opportunity cost, not coerced payment — decades of elite scholarly capacity and communal attention routed to non-performable law — and the reading's own lights endorse the arrangement, which caps how extractive it can honestly be called from this seat. Suppression (0.30) is mostly internalized and institutionally soft: no one is compelled by threat; the lock is curriculum, marriage market, livelihood, and self-concept. Theater ratio (0.22 and rising) reflects the growth of readiness pageantry — vessel reconstruction, priestly-genealogy auditing, red-heifer logistics — alongside a core scholarly function that remains genuine. Accessibility collapse is low (0.30): the sibling readings and outright exit remain live alternatives, and their continued availability is precisely what this reading must argue against. Resistance (0.25) is intermittent internal questioning plus external indifference. Enforcement is static across the interval — normative, curricular, stable — so no suppression_requirement series is authored; the scalar in base_properties carries the enforcement picture. Measurements run on one shared grid (t=0..60, approximately 1965-2025) with every tracked metric authored at every point; the rising trajectories track the postwar expansion of the academy (more capacity, hence larger opportunity cost) and the late-century activation of readiness activism (more pageantry). Boltzmann coordination type is identity_coordination: the function whose failure would matter most is the unraveling of the anticipatory identity binding dispersed communities to the restoration arc; knowledge preservation rides on that identity structure rather than the reverse.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the establishment seat the arrangement is what makes its lineage true — coordination it administers and embodies. From the student seat it is deferred consumption: formative decades priced into a payoff owned by others. From the diaspora-communal seat it is mostly subsidy: meaning and continuity received at modest direct cost. From the excluded seat it is indefensible deferral of the present to an undated future. The future-restored-community seat, were it computable, would sit at the pure-beneficiary pole — which is exactly why its interests are cheap to invoke and impossible to verify. The engine derives these divergences from the structural data; the authored scaffold claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. talmudic_students and present_need_bearers sit near the full-target end (high d), amplified by identity_locked and constrained exits respectively. diaspora_observant_communities and temple_readiness_movement sit near the beneficiary end (low d). future_restored_community is the terminal beneficiary (d near 0) — pure subsidy, contingent on restoration. One override is authored: the halakhic_scholarly_establishment would derive near-full-beneficiary from its beneficiary declaration alone, but it also bears the administration and enforcement costs of the arrangement it collects from, and its gains are institutional perpetuation rather than extracted rent; the true structural relationship sits nearer symmetric at d approximately 0.2, so an institutional-atom override is declared. Spatial scope is global — verifying 'readiness' claims is inherently hard at that scale, which the engine's scope modifier registers.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim keeps two misreadings apart. Read as pure coordination (rope), the deferral's asymmetric cost structure disappears: present generations pay in decades of non-performable expertise for a payoff contingent on an event no one controls. Read as pure extraction (snare), the genuine coordination function vanishes: the transmission chain is real, the preserved knowledge is real, and the identity continuity delivered is real. Mandatrophy status: the founding problem — keeping commandments that cannot be performed — is live under this reading and corroborated from outside the beneficiary set, so no zombie flag is asserted. But the sunset is eschatological: if restoration never obtains, the transitional justification hollows out while the machinery persists, and the degradation path from scaffold toward piton is exactly what the restoration_contingency omega tracks. The classification therefore blocks mislabeling in both directions while flagging the one condition under which the label itself would rot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_contingency,
    'Will the restoration condition ever obtain, firing the arrangement''s sunset and converting readiness back into performance?',
    'No empirical resolution is available; the condition is agent-independent and unfalsifiable in practice. The tradition''s own eschatological claims and the indefinite persistence of non-performance are the only observable signals.',
    'If restoration never occurs, the transitional justification hollows out while the machinery persists indefinitely — the degradation path from scaffold toward piton (maintenance without transition) — and this reading loses ground to its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency, conceptual, 'Irreducible uncertainty over whether the scaffold''s sunset condition is ever realized.').

omega_variable(
    opportunity_cost_magnitude,
    'How large is the true opportunity cost of routing elite scholarly capacity and communal attention into non-performable sacrificial law, relative to counterfactual present-facing uses?',
    'Longitudinal comparison of career and economic outcomes for kodashim-specialist scholars against matched cohorts in adjacent fields, plus communal budget-allocation analysis of readiness-directed giving.',
    'A large measured cost pushes effective extraction upward and the computed type toward tangled_rope or snare territory; a negligible cost supports the scaffold''s benign-transitional reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_magnitude, empirical, 'Size of the opportunity-cost transfer that constitutes this arrangement''s extraction.').

omega_variable(
    committer_reading_location,
    'This constraint is one reading of kernel kodashim_commandment_status — at which structural element do the readings disagree, and what would adopting a sibling change?',
    'Conceptual analysis of the sibling files: the disagreement is located in the residual normative force of the suspended commandment. study_as_performance relocates fulfillment into study itself; performance_only dissolves residual force into institutional contingency; this reading preserves obligation in abeyance pending restoration.',
    'Adopting study_as_performance converts the arrangement into steady-state fulfillment (lower epsilon, different victim set); adopting performance_only converts it into unmotivated maintenance of an inert husk (piton-flavored). The epsilon, victims, and type of this file are all reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Committer structure: location of the intra-kernel disagreement and its classification consequences.').

omega_variable(
    unscheduled_sunset_validity,
    'Does a sunset condition that is real and declared within the framework but unschedulable and agent-independent (messianic restoration) count as a declared sunset for scaffold classification?',
    'Comparative analysis against scheduled-transition scaffolds (emergency powers, development programs) and an operator ruling on contingent sunsets; the tradition''s own treatment of the restoration condition as definite-but-undated is the primary text.',
    'If contingent sunsets do not qualify, the scaffold claim weakens toward tangled_rope (enforced coordination plus asymmetric extraction with no operative transition) while all metric values remain unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unscheduled_sunset_validity, conceptual, 'Whether an eschatological sunset satisfies the scaffold''s sunset-clause requirement.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit of talmudic_students and the temple_readiness_movement structural (community embeddedness, marriage market, livelihood) or internalized (self-concept fused with the readiness mission)?',
    'Post-exit trajectory studies of students who leave the kodashim track and activists who abandon restoration preparation: if deference to the deferral frame persists after structural barriers are removed, the internalized component is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure — the lock travels with the agent after exit — and the victim seats compute nearer the full-target end than the raw structure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural versus internalized composition of the identity lock on the arrangement''s targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t10, kodashim_commandment_status__messianic_deferral, theater_ratio, 10, 0.12).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.13).
narrative_ontology:measurement(koda_tr_t30, kodashim_commandment_status__messianic_deferral, theater_ratio, 30, 0.16).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.18).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__messianic_deferral, theater_ratio, 50, 0.2).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(koda_be_t10, kodashim_commandment_status__messianic_deferral, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(koda_be_t30, kodashim_commandment_status__messianic_deferral, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__messianic_deferral, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__messianic_deferral, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% Kernel kodashim_commandment_status decomposes into three epsilon-invariant readings, authored as separate stories and linked here: messianic_deferral (this file, epsilon ~0.45 — obligation preserved in abeyance, study as readiness, moderate opportunity-cost extraction), study_as_performance (lower epsilon — study itself fulfills, steady-state occupation of the kernel), and performance_only (from that seat the same curriculum reads as guardianship of a husk — materially higher epsilon relative to value delivered). Each member has its own beneficiaries, victims, and claimed type; the shared upstream textual kernel is cited as warrant by all three, which is why the family edges run through this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
