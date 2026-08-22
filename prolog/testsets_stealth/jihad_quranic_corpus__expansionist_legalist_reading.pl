% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Legalist Jihad: Rule-Bound Expansion Mandate
 *   domain: religious/political_theology/comparative_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the jihad_quranic_corpus kernel:
 *   the classical legalist reading in which jihad is a communal obligation to
 *   establish Islamic governance where absent, exercisable offensively under
 *   rule-bound conditions — prior invitation to Islam, declaration only by
 *   legitimate imam authority, non-combatant immunity, proportionality, and
 *   truce law — with conquered populations incorporated through dhimma
 *   contracts. The story treats that reading as an OPERATING ARRANGEMENT
 *   (roughly the codification-to-post-caliphal arc, mapped onto interval
 *   0-30), not as a theological proposition: it has beneficiaries who
 *   collect, victims who pay, and enforcement machinery that holds it in
 *   place. The claim/metric gap is deliberate: the reading CLAIMS itself as
 *   divinely mandated rule-governed coordination; the authored metrics
 *   describe substantially extractive, actively enforced operation with
 *   rising performative maintenance. The engine measures that divergence;
 *   nothing here reconciles them. Sibling readings (defensive-spiritual,
 *   revolutionary-vanguard) are separate constraints with their own epsilon
 *   values and are NOT averaged into this file.
 *
 * KEY AGENTS:
 *   - caliphal_authority: agenda-setting beneficiary (institutional/arbitrage) — declares campaigns, collects the treasury's fifth and the tax rolls
 *   - ulama_jurisprudential_class: beneficiary (organized/identity_locked) — administers conditions and dhimma courts; authority constituted by the corpus
 *   - mujahidun_frontier_soldiers: dual-positioned payer-beneficiary (moderate/constrained) — bears mortality, collects spoils shares
 *   - frontier_non_muslim_polities: primary target (powerful/constrained) — organized states losing territory and tax base
 *   - dhimmi_communities: standing target with partial protection (powerless/constrained) — pays jizya under contingent terms
 *   - non_combatant_civilians_in_war_zones: rule-protected bystanders (powerless/trapped) — covered by immunity they cannot enforce
 *   - comparative_jurists_and_historians: analytical observer — sees the full structure across traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.73).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Legalist Jihad: Rule-Bound Expansion Mandate").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political_theology/comparative_law").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd').
narrative_ontology:cs_kernel_codification('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', fixed_text).
narrative_ontology:cs_authority_grounding('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', lineage).
narrative_ontology:cs_interpretation_layer_present('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd').
narrative_ontology:cs_reading_relation('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', foundational, governance_establishment_obligation).
narrative_ontology:cs_axiom_status(governance_establishment_obligation, holdable).
narrative_ontology:cs_axiom_grounding('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', governance_establishment_obligation, theological).
narrative_ontology:cs_axiom('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', foundational, imam_declaration_monopoly).
narrative_ontology:cs_axiom_status(imam_declaration_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', imam_declaration_monopoly, conventional).
narrative_ontology:cs_reference_frame('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', classical_caliphal_expansion_framework).
narrative_ontology:cs_drift_state('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eed9bd7d-0bc1-4234-a78b-22cc7c3b68bd', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurisprudential_class).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, mujahidun_frontier_soldiers).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, non_combatant_civilians_in_war_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, frontier_non_muslim_polities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_communities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, mujahidun_frontier_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares campaigns, appoints commanders, receives the treasury's fifth of movable spoils and the jizya and kharaj rolls, and alone holds treaty and truce power. Its legitimacy narrative rests on fulfilling the expansion mandate; it wrote the procedural rules and can suspend campaigns, purchase truces, or redirect effort at will.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Codifies the conditions (prior invitation, non-combatant immunity, spoils division, truce law), staffs the courts that administer dhimma contracts and tax collection, and collects standing authority, patronage, and judicial office from being the framework's authorized interpreters. Their scholarly standing is constituted by mastery of this corpus; repudiating it would dissolve the social position it grounds.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurisprudential_class, beneficiary,
    organized, generational, identity_locked, continental).

% Bear the marching, garrison duty, wounds, and mortality of the campaigns. They receive four-fifths shares of movable spoils, occasional land grants, and the spiritual merit the framework promises, but desertion carries penalties and garrison settlement binds them to the frontier. What flows to them and what flows from them are both substantial.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, mujahidun_frontier_soldiers, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, mujahidun_frontier_soldiers, beneficiary).

% Organized states on the receiving end of campaigns — Byzantine provinces, Sassanid successor territories, Iberian kingdoms. They lose borderlands, fortresses, and taxable populations; their options are military resistance, purchased truces, negotiated tribute, or eventual surrender on terms. None of these restores the pre-campaign status quo.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, frontier_non_muslim_polities, payer,
    powerful, generational, constrained, continental).

% Under surrender terms they retain worship, communal courts, and property, and pay jizya and land taxes; they are subject to dress, building, and public-office restrictions, and their protection is contingent on continued subordination. Exit paths exist — conversion (with heavy identity and inheritance costs), emigration, or revolt (which forfeits protection) — but all are costly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_communities, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_communities, beneficiary).

% Women, children, the elderly, and monastics whom the immunity rules forbid targeting during campaigns. They did not author the rules, cannot waive them, and cannot enforce them; their safety depends entirely on commander compliance and on the framework remaining in force.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_combatant_civilians_in_war_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Reconstruct the framework's operation from chronicles, legal treatises, and documentary papyri; compare its war-conduct terms with contemporaneous Byzantine and Latin practice; and trace how the doctrine traveled from conquest-era application to dormitory jurisprudence. They take no side in the obligation dispute.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_jurists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves three collective-action problems at once: organizing large-scale military mobilization under unified command (crowding out private raiding justified as piety), regulating the conduct of war through shared rules (prior invitation, non-combatant immunity, truce inviolability, proportional response), and standardizing the incorporation of conquered populations through dhimma contracts instead of unregulated slaughter or enslavement.
% TRANSFER_FUNCTION: Moves land, movable spoils, and standing tax revenue (jizya, kharaj) from non-Muslim populations outside and inside the frontier to the caliphal treasury and the fighting men; moves military labor from Muslim subjects to the frontier zones; and moves religious-legal authority and office to the jurist class that administers the framework.
% ABSENT_VOICES: The targeted non-Muslim populations have no seat in the jurisprudence that defines their options: the fiqh literature is authored entirely within the conquering polity, and dhimmi communities appear in it only as objects of regulation. Their objection — that the choice-set offered (convert, submit, fight, die) is itself the injury — survives only in their own chronicles and petition records, outside the framework's authoritative conversation.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, caliphal legitimacy loses its expansion mandate, the spoils-and-jizya fiscal order dissolves, dhimma arrangements collapse into either expulsion or unstructured incorporation, the jurist class loses the corpus that constitutes its authority, and frontier soldiers lose the reward structure that recruits them — the whole imperial-religious settlement reorganizes.
% FOUNDING_PROBLEM: The early Medinan polity faced simultaneous problems: raids threatened the community, treaty partners defected, and the new polity needed both defense and a lawful basis for mobilization beyond Arabia, plus a workable regime for governing conquered populations once expansion began. The jurists codified jihad to solve mobilization, legitimacy, and post-conquest governance in one structure.
% FOUNDING_PROBLEM_CORROBORATION: Byzantine chroniclers, Syriac and Coptic church histories, and Persian accounts attest the campaigns and their terms from the receiving end, corroborating the original security context and the subsequent shift to imperial routinization; modern academic historiography of the conquests independently documents both phases. No source outside the benefiting parties attests that the founding problem remains live today — the 'still-live' claim is voiced only by the framework's custodians.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the arrangement systematically moves land, spoils, and standing tax revenue from religiously-defined classes to the polity and its fighters; the rule-bound character (invitation, immunity, truce law) caps it well below unconstrained predation. Suppression (0.73) is structural and unscaled: persistence depends on the imam's monopoly on declaration — punishing unauthorized campaigning and desertion — and on enforcing jizya collection, not on participant preference. Theater rises monotonically across the interval (0.12 to 0.40): as expansion capacity waned in the later caliphal and post-caliphal periods, formal declarations, invitation embassies, and jurisprudential elaboration increasingly ran ahead of any campaign that would follow — Goodhart drift of the mandate into performance. The measurement series run on one shared time grid (T=0,6,12,18,24,30) so every tracked metric is authored at every examined point; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change (the monopoly hardening as consensus fragmented), not merely extraction shifting.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute very differently. From the caliphal and jurist positions the arrangement is a divinely authorized legal order they built, staff, and defend — genuine coordination with rule-bound conduct. From the frontier polities and dhimmi seats the same structure operates as enforced dispossession and standing taxation of a religiously-defined class. The soldier seat splits internally: mortality borne against spoils collected. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority sits nearest the beneficiary end (collects the fiscal flow, controls the rules, arbitrage-grade exit). The jurist class is a beneficiary with identity-locked exit — its authority is constituted by the corpus, so it defends the framework even where it no longer profits materially. Frontier polities and dhimmi communities sit near the full-target end: trapped or constrained targets bearing the transfer. Non-combatant civilians are genuine rule-beneficiaries with no enforcement capacity. The soldiers are the one seat the automatic derivation misreads: their appearance in the beneficiary array (spoils shares) would derive a strongly subsidized d, but they simultaneously bear the arrangement's mortality burden, placing them near symmetric. A directionality override (power_atom moderate, d_value 0.48) corrects this; they are the only moderate-power agent in the story, so the override touches exactly that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a besieged early polity and building a lawful mobilization and governance regime — was substantially solved and then superseded by the imperial fiscal order it created. The classification prevents two opposite mislabels: calling the whole structure a snare erases the real coordination function (war regulation, dhimma incorporation, mobilization discipline) that distinguished its conduct terms from contemporaneous alternatives; calling it a rope erases the asymmetric extraction (conquest transfers, standing confessional taxation) that disqualifies pure coordination. The rising theater_ratio traces the piton-ward drift of the mandate's outer shell — declarations without campaigns — while the fiscal and authority cores remained genuinely enforced, which is why the story resolves as tangled rope with documented drift rather than a settled category. The R5 mismatch consumer should note: founding_problem_status is contested while disappearance_verdict is world_rearranges — the arrangement still organizes seats even as its founding warrant is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Does the jihad_quranic_corpus actually ground the expansionist_legalist_reading instantiated here, or one of its siblings? If the defensive_spiritual_reading is better grounded in the revelation strata and earliest practice, this constraint''s beneficiary/victim structure dissolves into a defensive-war regime with far lower standing extraction; if the revolutionary_vanguard_reading, the imam-authority structure collapses and the obligation reattaches to individuals against rulers.',
    'Philological and chronological reconstruction: Meccan/Medinan strata of the relevant verses, the abrogation claims jurists used to sequence them, and the earliest attested practice records compared against later juristic construction.',
    'Determines which of three structurally different constraints the corpus supports; the epsilon, beneficiary set, and enforcement surface all move with the answer. This story''s classification is conditional on the allocation resolving toward the legalist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which reading of the jihad kernel the source corpus grounds — the committer-frame allocation question underlying this entire story.').

omega_variable(
    invitation_sincerity,
    'Did the prior-invitation precondition operate as a genuine choice-offering to targeted polities, or as a pro forma ritual step in campaigns already decided?',
    'Compare campaign chronology against embassy and correspondence records: intervals between invitation and invasion, terms offered, and whether refusal predictably triggered attack regardless of counteroffers.',
    'If invitations were predominantly sincere, part of the measured suppression reflects a real consent architecture and effective extraction falls; if pro forma, the invitation clause is theater and the arrangement sits closer to pure enforced transfer than its rules suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invitation_sincerity, empirical, 'Whether the da''wa precondition was functional consent machinery or ritual cover.').

omega_variable(
    dhimma_status_valence,
    'Did dhimma operate as protective incorporation — demonstrably better for religious minorities than the contemporaneous alternatives — or as institutionalized subordination: standing extraction from a religiously-defined class under contingent protection?',
    'Comparative fiscal and legal history using dhimmi-authored sources (church records, Geniza documents, petition archives) against the juristic literature, benchmarked against treatment of religious minorities in neighboring Byzantine and Latin jurisdictions.',
    'Flips the dhimmi seat''s weighting between beneficiary and victim sides and moves effective extraction materially; the dual-role declaration hedges but does not resolve this — the valence question decides which side dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimma_status_valence, conceptual, 'Whether the dhimma contract reads as protection or as institutionalized subordination.').

omega_variable(
    dormancy_vs_obsolescence,
    'Is the framework in the post-caliphal era dormant-valid (its conditions unmet, awaiting a legitimate imam, fully operative law) or effectively obsolete (historical jurisprudence retained only as inheritance)?',
    'Track how mainstream legalist institutions teach and apply the doctrine: operative-law status in curricula and fatwa practice versus historical-jurisprudence treatment, and whether revival movements are treated as continuous with the classical framework or as departures from it.',
    'If dormant-valid, the drift_state''s unacknowledged practice_drift masks a framework awaiting reactivation and the theater_ratio understates latent enforcement capacity; if obsolete, the arrangement is terminally declining and the rising theater_ratio is its wind-down signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_vs_obsolescence, conceptual, 'Whether the classical framework persists as live-but-unfulfilled law or as inherited jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jiha_tr_t6, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(jiha_tr_t12, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(jiha_tr_t18, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jiha_be_t6, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(jiha_be_t12, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(jiha_be_t18, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jiha_su_t6, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(jiha_su_t12, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(jiha_su_t18, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 30, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'jihad' decomposes into three structurally distinct constraints instantiating one kernel (jihad_quranic_corpus). This member (expansionist_legalist_reading) carries the highest standing extraction and the fullest institutional machinery; the defensive_spiritual_reading is upstream in textual-ambivalence terms (it is the reading the earliest strata most plausibly support, and this reading cites abrogation against it) while downstream the revolutionary_vanguard_reading borrows this reading's obligation-and-conditions machinery while rejecting its authority premise. Each file carries its own epsilon, beneficiaries, and victims; edges here record structural influence, not endorsement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
