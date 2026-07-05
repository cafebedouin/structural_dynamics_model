% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Systematization of Usul al-Fiqh (Hadith-Authentication Priority)
 *   domain: islamic_jurisprudence/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the Shafi'i reading of the usul_al_fiqh_method
 *   kernel: al-Shafi'i's systematization (notably in al-Risala) established a
 *   strict hierarchy where authenticated hadith must be exhausted before
 *   qiyas is permitted, restricted ijma to the Companions' generation alone,
 *   and elevated usul al-fiqh itself into a meta-discipline governing how any
 *   source may be used. This produced genuine coordination value — a shared,
 *   portable, auditable method across regions reduced the chaos of purely
 *   local or purely rationalist rulings — but it also structurally reassigned
 *   interpretive authority to hadith transmission specialists and
 *   Shafi'i-trained jurists, at the expense of rationalist jurists (Hanafi
 *   ra'y), Medinan practice-based jurists (Maliki 'amal), and local customary
 *   authorities. The rise of isnad criticism as a gatekeeping science is the
 *   mechanism: no legal conclusion can proceed to analogical reasoning until
 *   authentication specialists have ruled on the hadith in question, giving
 *   that specialist class veto power over what constitutes textual silence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.55).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Systematization of Usul al-Fiqh (Hadith-Authentication Priority)").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "islamic_jurisprudence/legal_theory").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'ac097112-d853-4247-b39f-9234194385c1').
narrative_ontology:cs_kernel_codification('ac097112-d853-4247-b39f-9234194385c1', formalized).
narrative_ontology:cs_authority_grounding('ac097112-d853-4247-b39f-9234194385c1', lineage).
narrative_ontology:cs_interpretation_layer_present('ac097112-d853-4247-b39f-9234194385c1').
narrative_ontology:cs_reading_relation('ac097112-d853-4247-b39f-9234194385c1', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac097112-d853-4247-b39f-9234194385c1', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac097112-d853-4247-b39f-9234194385c1', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('ac097112-d853-4247-b39f-9234194385c1', foundational, hadith_authentication_precedes_analogical_reasoning).
narrative_ontology:cs_axiom_status(hadith_authentication_precedes_analogical_reasoning, holdable).
narrative_ontology:cs_axiom_grounding('ac097112-d853-4247-b39f-9234194385c1', hadith_authentication_precedes_analogical_reasoning, conventional).
narrative_ontology:cs_axiom('ac097112-d853-4247-b39f-9234194385c1', foundational, ijma_restricted_to_companions_generation).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companions_generation, holdable).
narrative_ontology:cs_axiom_grounding('ac097112-d853-4247-b39f-9234194385c1', ijma_restricted_to_companions_generation, conventional).
narrative_ontology:cs_reference_frame('ac097112-d853-4247-b39f-9234194385c1', shafii_systematized_source_hierarchy).
narrative_ontology:cs_drift_state('ac097112-d853-4247-b39f-9234194385c1', post_classical_school_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ac097112-d853-4247-b39f-9234194385c1', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_school_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, isnad_criticism_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, hanafi_school_ray_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, regional_customary_law_authorities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, textual_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, companions_consensus_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certify isnad chains and authenticate hadith reports that become the prerequisite gate for any legal derivation. Their specialized expertise becomes indispensable once al-Shafi'i's hierarchy is adopted: no jurist can proceed to qiyas without first exhausting authenticated text, so the muhaddithun's judgments of authenticity control what downstream reasoning is even permitted. They can move between schools and regions carrying this expertise as portable capital.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter).

% Operate courts, issue fatwas, and train students within a methodology that privileges their systematized procedural discipline (usul al-fiqh as meta-science) over rival schools' looser or more locally-embedded reasoning. Their institutional legitimacy is built on and reproduced by the hierarchy; they cannot easily abandon it without dissolving their school's distinguishing claim to methodological rigor.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_school_jurists, beneficiary,
    institutional, civilizational, constrained, continental).

% Historically relied on ra'y and expansive qiyas to resolve cases where texts were silent or ambiguous, deriving authority from reasoned judgment and community-recognized competence. Under the Shafi'i hierarchy their analogical conclusions are subordinated and can be overturned whenever a hadith authenticator surfaces a report they must now defer to first. Their exit is constrained: they can migrate to Hanafi or Maliki institutional contexts, but cannot practice rationalist method within Shafi'i courts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Practice istihsan and expansive analogical reasoning as central method, treating textual silence as an invitation to reasoned discretion rather than a narrow residual category. Within Shafi'i-dominated jurisdictions their method is structurally disfavored; they are not part of the conversation that sets the Shafi'i hierarchy's terms and cannot contest it from within that framework — they can only operate where Hanafi institutions hold sway.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanafi_school_ray_practitioners, excluded,
    organized, generational, trapped, regional).

% Administer local custom ('urf) and practice-based norms that predate systematized hadith criticism. Under the Shafi'i hierarchy, custom carries no independent evidentiary weight unless it can be subsumed under an authenticated textual source or narrow qiyas; their accumulated local legal knowledge is devalued relative to isnad-certified transmission, and they have no realistic exit from jurisdictions where Shafi'i courts hold authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, regional_customary_law_authorities, payer,
    powerless, generational, trapped, local).

% The historical Companions' consensus (ijma) as restricted evidentiary category — not an acting party, but the fixed reference point the Shafi'i hierarchy invokes to close off later communal consensus claims as independently authoritative.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, companions_generation_memory, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__shafii_reading, companions_generation_memory).

% Later scholarly communities whose collective agreement on legal questions would, under a broader ijma doctrine, carry independent weight. The Shafi'i restriction of ijma to the Companions' generation forecloses their consensus from functioning as an independent source, channeling their authority instead through hadith-mediated derivation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, later_generation_jurist_communities, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, auditable hierarchy of legal sources (Quran, authenticated Sunna, restricted ijma, qiyas as residual) that lets jurists across regions and generations converge on a shared method for deriving rulings, reducing arbitrary or purely idiosyncratic reasoning and enabling cross-regional legal coherence.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional prestige from jurists whose legitimacy rested on reasoned discretion (ra'y, expansive qiyas, local custom) to jurists whose legitimacy rests on hadith authentication expertise and procedural mastery of usul al-fiqh; case outcomes and consequently litigant and community deference shift toward Shafi'i-trained scholars and hadith specialists.
% ABSENT_VOICES: Hanafi ra'y practitioners and Maliki 'amal-based jurists are not present as co-authors of the Shafi'i hierarchy; they would object that expansive qiyas and Medinan practice carry independent evidentiary legitimacy the hierarchy denies them. Regional customary authorities are similarly absent — their local knowledge predates and falls outside the systematized framework entirely.
% DISAPPEARANCE_RATIONALE: If the Shafi'i hierarchy's authority were to vanish, jurists would revert to (or elevate) rival methodologies — expansive qiyas and istihsan (Hanafi), Medinan practice and maslaha mursala (Maliki) — as independently sufficient sources without needing to first exhaust hadith authentication; hadith transmission specialists would lose their gatekeeping leverage over legal derivation, and courts organized around Shafi'i procedural priority would need to reconstitute their method or cede ground to rival schools.
% FOUNDING_PROBLEM: Early legal reasoning across regions was fragmented and inconsistent — jurists in different cities reached divergent rulings using ra'y, local custom, and loosely sourced traditions, with no shared standard for what counted as valid evidence or how sources ranked against each other, risking arbitrary or contradictory law under a single religion.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i-tradition jurists and hadith scholars attest the problem remains live: without rigorous authentication priority, legal derivation risks capture by unverifiable opinion. Historians of Islamic law and comparative legal scholars outside the Shafi'i tradition (including scholars documenting Hanafi and Maliki institutional histories) attest that the fragmentation problem was real at founding but that the specific hierarchy adopted also functioned to concentrate interpretive authority in hadith-transmission specialists — a shift in institutional power, not solely a methodological fix.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).
:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) — the hierarchy is not primarily a rent-extraction device; it does real coordination work by making legal derivation auditable and cross-regionally portable. But it is not costless: it reassigns interpretive authority away from rationalist and customary jurists toward hadith specialists, and this reassignment persists via ongoing enforcement (school affiliation, court staffing, curricular requirements) rather than through neutral competition among methods. Suppression (0.55) reflects that once the hierarchy is institutionally embedded, alternative methodologies are not merely disfavored in argument but structurally excluded from Shafi'i courts. Theater ratio is modest (0.28) — most of the apparatus (isnad criticism, chains of transmission, procedural usul al-fiqh) performs a real function, though some proportion of scholarly isnad disputation has calcified into credentialing ritual disconnected from live legal questions.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists and Shafi'i jurists sit near the beneficiary end: they gain gatekeeping power and portable institutional capital, and their exit options (arbitrage across regions carrying recognized expertise) are strong. Rationalist jurists and regional customary authorities sit near the target end: their accumulated interpretive and customary authority is devalued by the hierarchy's terms, and their exit options are constrained or trapped by jurisdiction. The Companions'-generation memory is a non-agent reference point, not a stakeholder collecting anything — it is listed for completeness only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, inconsistent legal reasoning with no shared evidentiary standard) is genuinely contested as to whether it remains live in the same form today established Islamic legal traditions have long since stabilized around one hierarchy or another, and the fragmentation risk that justified the original systematization is not obviously still acute in the same way. Because corroboration for the founding problem's continued vitality comes disproportionately from within the tradition that benefits from the hierarchy (Shafi'i jurists, hadith scholars), while outside historians of comparative Islamic law describe a genuine institutional power shift alongside the coordination gain, this is exactly the kind of story the tangled_rope classification exists to hold: real coordination function AND asymmetric extraction, both true at once, neither cancelling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture_boundary,
    'Is the Shafi''i hierarchy''s restriction of qiyas and ijma primarily a genuine solution to methodological chaos, or primarily a mechanism by which hadith-transmission specialists captured interpretive authority that had previously been distributed among rationalist and customary jurists?',
    'Comparative historical analysis of legal outcome consistency before and after widespread Shafi''i institutional adoption, cross-checked against the career and wealth trajectories of hadith-transmission specialists relative to rationalist jurists in regions where the hierarchy was adopted versus regions where it was not.',
    'If capture dominates, the tangled_rope classification should weight toward the snare end; if coordination dominates, toward rope. The current 0.42 extractiveness reflects a genuine middle judgment given uncertain evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_boundary, conceptual, 'Whether the Shafi''i source hierarchy is best read as coordination-with-extraction or as extraction wearing a coordination justification.').

omega_variable(
    kernel_reading_incommensurability,
    'Is ''usul al-fiqh method'' genuinely a single contested kernel with four readings (Hanafi, Maliki, Shafi''i, Hanbali), or are these better modeled as four historically and doctrinally independent methodologies that only appear to share a kernel because they address a common set of source-hierarchy questions?',
    'Textual and historical analysis of whether the four schools'' founders treated each other''s methods as competing answers to the same question (supporting a shared kernel) or as answering fundamentally different questions about legal epistemology (supporting independent constraints).',
    'If the schools are genuinely independent rather than readings of one kernel, the reading_relations declared here (coexists_with, influences) may overstate structural connection; each would instead be evaluated as a freestanding constraint with only historical, not logical, relationship to the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the four fiqh schools instantiate one contested kernel or four independent constraints.').

omega_variable(
    companions_ijma_restriction_naturalness,
    'Is restricting ijma to the Companions'' generation a principled epistemic claim (their proximity to revelation gives their consensus unique evidentiary weight) or a strategic move to foreclose later communities'' consensus from ever independently overriding hadith-mediated rulings?',
    'Analysis of al-Shafi''i''s own stated rationale in al-Risala against the practical effect of the restriction on later jurists'' ability to establish independent communal consensus claims.',
    'If strategic, this strengthens the case that later_generation_jurist_communities are structurally excluded victims rather than merely doctrinally disagreeing parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(companions_ijma_restriction_naturalness, conceptual, 'Whether the Companions-only ijma restriction is principled epistemology or authority foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__shafii_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__shafii_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__shafii_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__shafii_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__shafii_reading, theater_ratio, 1000, 0.27).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__shafii_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 200, 0.3).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__shafii_reading, base_extractiveness, 400, 0.36).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__shafii_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__shafii_reading, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__shafii_reading, suppression_requirement, 400, 0.48).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__shafii_reading, suppression_requirement, 600, 0.51).
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__shafii_reading, suppression_requirement, 800, 0.53).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1000, 0.54).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'usul al-fiqh method' per the ε-invariance principle: hanafi_reading (expansive qiyas/istihsan), maliki_reading (Medinan practice/maslaha mursala), shafii_reading (this story: hadith-authentication priority, restricted ijma), and hanbali_reading (maximal textual restriction). Each has its own ε, beneficiaries, victims, and classification; they are linked here as a constraint family, not merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
