% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Messianic Suspension of the Sacrificial Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the sacrificial commandments
 *   remained textually intact while becoming physically impossible to
 *   perform. The messianic_suspension reading holds that these obligations
 *   are SUSPENDED — not fulfilled, not violated, not dissolved — pending a
 *   restoration event that restores both the site and the possibility of
 *   valid performance. Under this reading the standing arrangement is a
 *   deferral regime: halakhic adjudication rules that nothing is owed now and
 *   cannot be done now; the community's practical religion proceeds without
 *   sacrificial performance; and the corpus of sacrificial law is kept
 *   teachable so that an intact obligation, not a reconstructed one, awaits
 *   restoration. Study of the sacrificial orders is, on this reading, neither
 *   compliance nor preparation-for-compliance but maintenance of
 *   knowledge-in-waiting — the file is kept readable, not acted on. KEY
 *   AGENTS (by structural relationship): - halakhic_adjudicators:
 *   Administrator of the deferral (institutional / identity_locked) — rules
 *   that nothing currently binds, maintains the dormant corpus, defers final
 *   adjudication to an event it cannot cause - diaspora_jewish_communities:
 *   Primary beneficiary (organized / constrained) — relieved of
 *   undischargable duties; rehearses restoration liturgically -
 *   priestly_lineages: Secondary beneficiary (moderate / identity_locked) —
 *   hold dormant hereditary service status that survives only inside the
 *   deferral - restorationist_movements: Principal cost-bearer (powerless /
 *   constrained) — denied performance of a duty they hold live -
 *   academic_historians_of_religion: Analytical observer — sees the full
 *   structure from outside the tradition's self-description
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, scaffold).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Messianic Suspension of the Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a').
narrative_ontology:cs_kernel_codification('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', fixed_text).
narrative_ontology:cs_authority_grounding('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', lineage).
narrative_ontology:cs_interpretation_layer_present('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a').
narrative_ontology:cs_reading_relation('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', foundational, obligation_persists_through_catastrophe).
narrative_ontology:cs_axiom_status(obligation_persists_through_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', obligation_persists_through_catastrophe, theological).
narrative_ontology:cs_axiom('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', foundational, deferred_application_preserves_faithfulness).
narrative_ontology:cs_axiom_status(deferred_application_preserves_faithfulness, holdable).
narrative_ontology:cs_axiom_grounding('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', deferred_application_preserves_faithfulness, deontological).
narrative_ontology:cs_reference_frame('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', operative_temple_service_order).
narrative_ontology:cs_drift_state('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', contemporary_post_site_access_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7cd1e13f-bfa2-4f0a-9b70-de9cae468b3a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, priestly_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__messianic_suspension, restorationist_movements).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, obligation_inviolability_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decisor courts and ordination lineages rule that the sacrificial commandments do not currently bind and cannot validly be performed, and they maintain the interpretive apparatus — the mishnaic orders, Talmudic tractates, and responsa on sacrifice — that would govern them again upon restoration. They cannot cause the restoration themselves; their office consists in keeping the deferred file open and answering adjacent questions (mourning rites, pilgrimage language, priestly dues) as they arise. Their standing rests on continuity of transmission, so abandoning the framework would end the office rather than reform it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_adjudicators, agenda_setter,
    institutional, generational, identity_locked, global).

% Live full covenantal lives — prayer, festivals, dietary and lifecycle practice — in the Temple's absence. Because the sacrificial commandments are held in suspension, no unperformable duty hangs over them; they rehearse restoration in daily liturgy, mourn the loss on calendrical fasts, and fund the schools and courts that keep the suspended corpus teachable. Leaving the community through assimilation or conversion is possible but carries deep familial and social cost for most members.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Families of hereditary priestly descent hold a service role that has lain dormant for the whole interval. They keep selected purity disciplines, maintain genealogical records, and receive customary honors in synagogue ritual whose stated rationale connects to Temple service. Their vocation survives only inside the deferral — dissolution of the suspended obligation would dissolve the role it preserves — and their status is fixed by descent, so exit is not available to them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, priestly_lineages, beneficiary,
    moderate, generational, identity_locked, global).

% Small groups, mostly centered in Israel, hold that sacrificial performance is due now and prepare for it: breeding candidate animals, fabricating vessels, petitioning for site access. Rulings on ritual impurity and site sovereignty block performance, and their petitions are repeatedly declined by the adjudicating mainstream. They carry the frustration of a duty they believe is live; standing down is possible, and some factions periodically do.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, restorationist_movements, payer,
    powerless, biographical, constrained, regional).

% Study the post-destruction adaptation comparatively, alongside Samaritan, Karaite, and Christian handling of the same rupture. They take no position inside the adjudication, publish on the mechanics of religious deferral, and observe the full structure from outside the tradition's self-description.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, academic_historians_of_religion, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__messianic_suspension, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a covenantal community coherent under a condition it did not choose: central commandments rendered physically unperformable while remaining textually intact. The suspension supplies one uniform answer — the duties are neither void nor counterfeitably performed — so that no member must decide alone whether to abandon, fake, or agonize over them, and communal religious life proceeds on a shared footing.
% TRANSFER_FUNCTION: Moves almost nothing material. It holds the obligation in escrow — transferring its discharge from the present generation to a future restored community — transfers interpretive custody of the dormant corpus to the adjudicating class, and converts would-be performance into anticipatory labor: study, liturgy, genealogy, and vessel-crafting that maintain readiness without discharging anything.
% ABSENT_VOICES: Restorationist voices sit at the adjudication's margin: they object that performance is due now and are answered rather than seated. Secular Jews, for whom the covenantal frame is not authoritative, are not consulted at all. Samaritan and Karaite communities run parallel answers to the same rupture without participating in this arrangement's adjudication. And no seat can speak for the obligation itself — whether suspension honors or injures the commandments it holds is a question the conversation structurally cannot hear.
% DISAPPEARANCE_RATIONALE: If the deferral regime vanished overnight, the community would confront the raw problem it solves: either the commandments bind now, and every member stands in recognized violation with no remedy, or they are void, and the liturgy's restoration petitions, the priesthood's surviving rationale, the mourning calendar, and the sacrificial-study curriculum all lose their object at once. Congregations would split between those two answers, priestly honors and purity disciplines would lapse, and the daily liturgy would require rewriting. Arrangements across the community demonstrably depend on the deferral.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the sacrificial commandments remained textually binding while becoming physically impossible to perform. The founding problem: how can a covenantal community remain faithful to commandments it cannot discharge — without declaring them void, and without counterfeiting their performance?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Roman-era historians (Tacitus; Josephus writing under Roman patronage) attest the destruction independently of rabbinic tradition; the archaeological record of the burned Temple precinct and the Arch of Titus spoils depiction confirm the loss of the performance site; the site's successive imperial custodianships document the continuing physical impossibility; and the Samaritan community's divergent answer to the identical rupture shows that the problem itself — not merely this solution — is real.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08 at interval end) because the arrangement collects essentially nothing from anyone: no performance is owed, no payment is taken, and the residual costs (priestly purity discipline, scholarly maintenance of the corpus, liturgical rehearsal) are small and largely self-imposed. Suppression is near-floor (0.05): nothing is coerced, rival readings are taught openly inside the same institutions, and parallel communities run competing answers without interference. Accessibility collapse is low (0.15) because alternatives do NOT close off — the sibling readings, prayer-substitution theologies, and activist restorationism all remain live and practiced, which is precisely why this story is one reading of a contested kernel rather than the kernel itself. Resistance is low (0.10): there is almost nothing here to resist, and the fringe that does resist is small.
 *   
 *   The measurement series run on one shared eight-point grid (both metrics authored at every point). Base extractiveness traces a shallow U: elevated at the destruction-generation (0.12 — acute adjudication of substitutes, coercive episodes over prayer-form disputes, real adjustment costs), flattening to near-zero across the long middle (0.05–0.07), ticking up in the modern era (0.08) as restorationist friction and the political salience of the Temple site raise the deferral's marginal cost. Theater ratio rises monotonically (0.08 to 0.28): as historical distance grows, an increasing share of activity around the suspended corpus is rehearsal-shaped — daily liturgical petitions, fast-day mourning, reconstructed vessels, sacrificial-passages recital. Crucially, on THIS reading that rehearsal is functional rather than vestigial: it is the mechanism by which the suspended obligation is kept salient enough to be resumed rather than quietly forgotten. The ratio is therefore authored honestly as rising but sub-piton, and the commentary defends why the rise does not signal atrophy.
 *   
 *   No suppression_requirement series is authored: the enforcement picture is static and near-zero across the whole interval (there is no enforcement machinery to build up or erode), so the story-level scalar carries that fact, per the static-enforcement rule.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the restorationist seat, the arrangement is a denial regime: a duty they hold live is blocked, and the block is administered by the very authority that claims custodianship of the duty — a constraining, costly structure. From the community and priestly seats, the same structure is relief and preservation: duties no one could discharge impose no burden, and a dormant vocation survives intact instead of being dissolved. From the adjudicator seat, it is stewardship: an open file responsibly maintained. The engine derives these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: diaspora_jewish_communities and priestly_lineages are declared beneficiaries and derive low directionality (the arrangement subsidizes them — relief and preserved status respectively). restorationist_movements sits as payer on the stakeholder surface: the deferral's one real present cost lands there, giving that seat high directionality despite its small size — which is why no victims[] array is declared at base_properties (the mainstream structure has no victim set) while the payer seat is still honestly named. halakhic_adjudicators receive an explicit override (institutional, d=0.5): they appear in no beneficiary or victim declaration because they administer rather than collect, so the structural derivation would fall back to a generic institutional default; their true position is symmetric — they bear the labor of maintaining the corpus and gain custodial standing in equal small measure. The academic observer is analytical and outside the computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to remain faithful to commandments that cannot be discharged — is still live: it recurs every generation the Temple stays unbuilt, and the arrangement's daily operations (liturgy, mourning calendar, corpus maintenance) are answers to it, not residues of it. No mandate has outlived its function, so no mandatrophy is declared. The receipt surface nonetheless places this story adjacent to the piton cell (diffuse gains, prohibitive fixing cost), and the analysis must say why that adjacency fails. The piton test is cost-asymmetry plus atrophied function: an administrator who could change a constraint but bears too little of its cost to bother. Here the administrator COULD nominally dissolve the deferral (declaring the obligations inoperative), and the fix is prohibitive — but prohibitive not because of inertia or capture: removal would contradict the arrangement's own constitutive commitment (the obligation must survive intact for restoration), dissolve the priesthood's surviving rationale, orphan the liturgy's central petitions, and split the community between 'binding now' and 'void.' The theater that accumulates serves the deferral function rather than substituting for it. What looks like vestigial drag is the holding pattern doing exactly what the reading says it does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_ambiguity,
    'This constraint instantiates only the messianic_suspension reading of the temple_sacrifice_obligation kernel. Do the sibling readings (study_as_occupation, study_as_archiving) describe the same underlying arrangement with different bookkeeping, or structurally different constraints with different epsilon, beneficiary sets, and failure modes?',
    'Comparative analysis of what each reading obligates NOW: if any sibling generates a present-duty (study-as-discharge) or a present-victim set, it is a distinct constraint; if all three converge on ''nothing is owed now,'' they are descriptions of one arrangement.',
    'If the siblings are distinct constraints, this story''s epsilon and stakeholder surface are correct as authored; if they are one arrangement, the corpus should merge them and recompute classification over the union.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_ambiguity, conceptual, 'Whether sibling readings of the sacrifice-obligation kernel are separate constraints or one arrangement described differently.').

omega_variable(
    eschatological_sunset_actuality,
    'The arrangement carries a declared termination condition (restoration of the Temple), but the trigger lies outside every party''s control and has stood unmet for the full interval. Does a sunset clause no party can actuate still make the arrangement transitional, or does indefinite deferral convert it de facto into a steady-state coordination regime?',
    'Conceptual analysis cross-checked against comparable indefinite-sunset regimes (provisions ''until return of conditions X''): do such regimes behave as bridges (winding-down behavior, preparation for termination) or as permanent fixtures?',
    'If the sunset is functionally inert, the transitional justification fails and the arrangement should be read as a steady-state coordination solution rather than a holding pattern; the claimed type and the sunset-flag''s weight both change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_sunset_actuality, conceptual, 'Whether an unactuable, indefinitely deferred sunset clause still grounds transitional status.').

omega_variable(
    revealed_restoration_expectation,
    'Do adherents treat restoration as a live expectation that structures present behavior, or as a liturgically affirmed but practically never-expected event?',
    'Revealed-preference evidence at political openings: after 1967, when partial access to the Temple Mount became possible, communities overwhelmingly did not move to resume performance and mainstream adjudication reaffirmed the bar; survey and behavioral data on restoration expectation versus practice.',
    'Live expectation supports the transitional reading (the arrangement is awaiting activation); dead expectation suggests the deferral has become the terminal state, pushing toward steady-state classification and raising the theater ratio''s diagnostic weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revealed_restoration_expectation, empirical, 'Whether restoration expectation is behaviorally live or liturgically nominal.').

omega_variable(
    restorationist_cost_attribution,
    'Restorationist movements bear the arrangement''s clearest present cost (denied performance of a duty they hold live). Is that cost imposed BY the suspension regime on parties inside it, or do restorationists stand outside the arrangement by their own doctrinal choice, making their position self-selected rather than borne?',
    'Examine whether the mainstream adjudication claims jurisdiction over restorationist practice (rulings addressed to them, refusals framed as binding) or treats them as outside the frame; compare with how the arrangement treats other non-conforming groups.',
    'If the cost is imposed, a victim set exists and the arrangement''s profile shifts toward hybrid coordination-plus-denial; if self-selected, the no-victim structure stands and the low-extraction profile is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restorationist_cost_attribution, conceptual, 'Whether the restorationist fringe is a victim set of the arrangement or self-excluded from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_msus_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tso_msus_tr_t0, observed).
narrative_ontology:measurement(tso_msus_tr_t250, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 250, 0.1).
narrative_ontology:measurement_basis(tso_msus_tr_t250, observed).
narrative_ontology:measurement(tso_msus_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.12).
narrative_ontology:measurement_basis(tso_msus_tr_t500, observed).
narrative_ontology:measurement(tso_msus_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.15).
narrative_ontology:measurement_basis(tso_msus_tr_t1000, observed).
narrative_ontology:measurement(tso_msus_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.17).
narrative_ontology:measurement_basis(tso_msus_tr_t1500, observed).
narrative_ontology:measurement(tso_msus_tr_t1800, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1800, 0.2).
narrative_ontology:measurement_basis(tso_msus_tr_t1800, observed).
narrative_ontology:measurement(tso_msus_tr_t1900, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1900, 0.24).
narrative_ontology:measurement_basis(tso_msus_tr_t1900, observed).
narrative_ontology:measurement(tso_msus_tr_t1955, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1955, 0.28).
narrative_ontology:measurement_basis(tso_msus_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(tso_msus_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(tso_msus_be_t0, observed).
narrative_ontology:measurement(tso_msus_be_t250, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 250, 0.09).
narrative_ontology:measurement_basis(tso_msus_be_t250, observed).
narrative_ontology:measurement(tso_msus_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.07).
narrative_ontology:measurement_basis(tso_msus_be_t500, observed).
narrative_ontology:measurement(tso_msus_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.06).
narrative_ontology:measurement_basis(tso_msus_be_t1000, observed).
narrative_ontology:measurement(tso_msus_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement_basis(tso_msus_be_t1500, observed).
narrative_ontology:measurement(tso_msus_be_t1800, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement_basis(tso_msus_be_t1800, observed).
narrative_ontology:measurement(tso_msus_be_t1900, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1900, 0.07).
narrative_ontology:measurement_basis(tso_msus_be_t1900, observed).
narrative_ontology:measurement(tso_msus_be_t1955, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1955, 0.08).
narrative_ontology:measurement_basis(tso_msus_be_t1955, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle. The colloquial label 'what happened to the sacrifice obligation after the Destruction' conflates at least two structurally distinct claims: (a) the STATUS of the obligation now (instantiated here as messianic_suspension: suspended, nothing owed, epsilon near floor, no victim set) and (b) the FUNCTION of study relative to the obligation (instantiated by the two sibling stories: discharge versus archival preservation, each with its own epsilon and stakeholder consequences). The upstream story is this one: the suspension premise is what gives the sibling study-readings their operating environment — if nothing is owed, study cannot be discharge and must be something else. Each family member links to the others via affects_constraints; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__messianic_suspension, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
