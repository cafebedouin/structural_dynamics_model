% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Generational Transmission Criterion for Language Vitality
 *   domain: sociolinguistic/political/religious
 *
 * SUMMARY:
 *   This story authors the native-generation criterion for linguistic
 *   vitality as a single, epsilon-invariant arrangement: a language counts as
 *   living only if native speakers transmit it generationally as a mother
 *   tongue in daily life, and practices that fall short of that test —
 *   however ancient or continuous — are classified as preservation rather
 *   than vitality. The criterion is genuinely coordinative: producing a
 *   native-speaker cohort where none exists requires standardized planning,
 *   immersion schooling, and household alignment that no family achieves
 *   alone. It is simultaneously extractive: the test strips standing from
 *   liturgically maintained traditions, channels public money and careers
 *   toward transmission bureaucracies, and concentrates the honorific of
 *   'rescuer' on the movements that wrote the test. This file instantiates
 *   one reading of a decomposed label; see kernel_context for the reading
 *   structure and network.dual_formulation_note for the family decomposition.
 *   The claim (tangled_rope) and the metrics are authored independently: the
 *   metrics describe the arrangement's actual operation, and the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - secular_nationalist_movements: agenda-setter and principal beneficiary (organized / identity_locked) — codifies the criterion in charters and education policy; collects legitimacy via linguistic sovereignty; exit would mean abandoning the nation-defining project
 *   - - language_revival_institutions: administrative beneficiary and co-agenda-setter (institutional / constrained) — runs academies, immersion systems, and certification; accrues budgets, posts, and the authority to decide who counts
 *   - - state_culture_ministries: funding beneficiary (institutional / mobile) — converts the criterion into a measurable nation-building deliverable and a defensible budget line
 *   - - liturgical_only_communities: primary target (organized / identity_locked) — unbroken ritual transmission across centuries reclassified as curation of a corpse; bears the full symbolic cost with no offsetting gain
 *   - - partial_transmission_households: secondary target with an intra-family benefit stream (moderate / constrained) — bears the domestic labor of transmission yet is scored as failing below the native threshold
 *   - - international_heritage_bodies: observer (institutional / analytical) — lends the criterion global statistical form through vitality indices
 *   - - sociolinguists: analytical observer (analytical / analytical) — documents the continua that strain the criterion's binary core
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.62).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generational Transmission Criterion for Language Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistic/political/religious").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'aa03f158-57de-4b99-bd1a-4a8c49aa1b51').
narrative_ontology:cs_kernel_codification('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', distributed).
narrative_ontology:cs_authority_grounding('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', extraction).
narrative_ontology:cs_interpretation_layer_present('aa03f158-57de-4b99-bd1a-4a8c49aa1b51').
narrative_ontology:cs_reading_relation('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', foundational, generational_mother_tongue_transmission_necessary).
narrative_ontology:cs_axiom_status(generational_mother_tongue_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', generational_mother_tongue_transmission_necessary, empirically_contingent).
narrative_ontology:cs_axiom('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', secondary, liturgical_recitation_preserves_corpus_not_vitality).
narrative_ontology:cs_axiom_status(liturgical_recitation_preserves_corpus_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', liturgical_recitation_preserves_corpus_not_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', herderian_vernacular_norm).
narrative_ontology:cs_drift_state('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', contemporary_new_speaker_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa03f158-57de-4b99-bd1a-4a8c49aa1b51', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, language_revival_institutions).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_culture_ministries).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, partial_transmission_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, partial_transmission_households).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, critical_period_acquisition_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, ethnolinguistic_nationhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies the vitality criterion in revival charters, party platforms, and education policy, and campaigns for its adoption by the state. When the criterion is written into law or curriculum, the movement holds the recognized title of the language's rescuer and the nation gains a sovereign linguistic credential. Departure from the project would mean surrendering the nation-defining narrative the movement is built on, so exit is not a live option for its cadres.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_movements, beneficiary).

% Operate academies, terminology committees, immersion school networks, and speaker-certification schemes. Budget lines, teaching posts, and publishing programs are sized to the number of children acquiring the language at home, so institutional growth tracks the criterion's strictness. These bodies draft the operational tests — household language surveys, fluency gradings — that decide who counts as transmitting the language.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, language_revival_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, language_revival_institutions, agenda_setter).

% Fund transmission infrastructure and report native-speaker headcounts upward as a nation-building deliverable. The criterion supplies a measurable target and a defensible budget rationale; replacing it with vaguer multi-factor assessment would weaken the reporting line. Ministries can redirect funds comparatively easily if political priorities change.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_culture_ministries, beneficiary,
    institutional, generational, mobile, national).

% Maintain the language through daily prayer, scriptural study, and ritual observance, with transmission running through schools and worship rather than the kitchen table; many have kept the chain unbroken for centuries across dispersal. Under a household-transmission test their practice registers as zero, and public classification events describe them as curators of a dead tongue. The practice is constitutive of who they are; converting to vernacular domestic use is not something they consider, and they contest the verdict in print and, where language policy is justiciable, in court.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    organized, civilizational, identity_locked, global).

% Families, often in mixed-language marriages or designated revival districts, raising children with partial fluency — the language spoken at home alongside a dominant language, with comprehension outpacing production. They supply the domestic labor the criterion demands but fall short of its threshold, so surveys record them as failures even where their children retain more than their grandparents did. Their options are intensifying home use at real cost to household ease and majority-language schooling, or accepting the deficit classification.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, partial_transmission_households, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, partial_transmission_households, beneficiary).

% Publish vitality assessments and endangered-language atlases that rank intergenerational transmission as the leading indicator, lending the criterion global statistical form. They aggregate member-state data, convene expert panels, and periodically widen the factor set under academic pressure.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, international_heritage_bodies, observer,
    institutional, generational, analytical, global).

% Study acquisition, shift, and revitalization; document new-speaker continua, semi-speakers, and latent bilingualism that strain the native/non-native binary. They publish critiques and alternative indices but hold no enforcement power, and several advise the very institutions they criticize.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, language_revival_institutions).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the conversion of a language with few or no home transmitters into one with an intergenerational native-speaker base: standardizing orthography and terminology, building immersion schooling, and aligning household language choice so that a child cohort actually acquires the language as a mother tongue.
% TRANSFER_FUNCTION: Moves public funding, teaching careers, and certification authority toward native-transmission infrastructure; moves the honorific status 'living language' — and the policy deference attached to it — away from liturgically maintained traditions and toward movements capable of producing native-speaker cohorts.
% ABSENT_VOICES: Liturgical practitioners and elderly fluent second-language speakers are seldom seated when vitality criteria are codified in revival congresses and ministry consultations; diaspora households sustaining partial transmission have no seat in all-or-nothing threshold debates; children assigned to immersion regimes are represented only by adults.
% DISAPPEARANCE_RATIONALE: Funding formulas keyed to native-speaker counts would lapse; census and school-enrollment categories would reorganize; liturgical communities' continuity claims would regain equal standing; revival movements would lose the metric distinguishing 'revival' from 'maintenance' and would have to justify their infrastructure on service-delivery grounds alone.
% FOUNDING_PROBLEM: Nineteenth-century nationalist movements confronted languages deemed incapable of carrying a modern nation because no community spoke them at home. The criterion was built to force the question 'does this language have vernacular life?' and to make revival — not custodial maintenance — the only path that answers it affirmatively.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the empirical sociolinguistics of language shift treats intergenerational transmission as the pivotal stage of language death and retention, and international vitality assessments independently rank it as the leading factor — neither source is a nationalist beneficiary. Period philological dismissals of 'dead' scholarly languages are documented in sources outside the revival movements themselves.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.58 (moderate, per the manifest bin) with the referent fixed as the standing arrangement under contest: the native-transmission criterion as deployed in revival polities, assessed by this reading's own lights — never the pluralist or liturgical alternative. Extraction is real but bounded: where transmission infrastructure succeeds it produces the very asset (native cohorts) the criterion demands, so a substantial share of the cost is the price of the coordination itself. Suppression is 0.62 and is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Suppression operates through classification gates (census categories, funding eligibility, official vitality rankings) rather than prohibition; rival criteria are not banned, they are defunded and renamed as failure. Theater_ratio is 0.28: ceremonial 'first native speaker' commemorations, anniversary pageantry, and census self-report inflation are real but sit atop functioning transmission work (immersion graduates, terminology output). Accessibility_collapse is 0.50: within a polity that has adopted the criterion, alternative vitality claims collapse domestically, but the sibling criteria persist in other jurisdictions and among domestic dissenters, so alternatives are suppressed, not eliminated. Resistance is 0.55: liturgical communities rebut the framing explicitly, scholars contest the binary, and some revivalists hedge their own criterion. The temporal series run on one shared grid (1880, 1910, 1940, 1970, 2000, 2025) with every tracked metric authored at every point; the arc is monotone intensification through state capture (1880-1970), a theater peak around 2000 as counting games mature, and modest easing by 2025 under multi-factor frameworks and new-speaker critique. The dynamics are drift, not cycles; no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (movements, academies, ministries) the arrangement is a rescue mission's scoreboard: the criterion is what makes revival distinguishable from sentiment, and every native-speaker statistic is a victory. From the liturgical seat the identical criterion is defamation administered by institutions — a test rigged so that a millennium of unbroken practice scores zero. From the household seat it is a moving goalpost: the labor is compulsory, the threshold unreachable, and the classification arrives as a verdict on the family. From the ministry seat it is a KPI. The engine computes these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (movements, revival institutions, ministries) derive directionality near the beneficiary end: the criterion subsidizes them with legitimacy, budgets, and certification authority, and their exits are poor precisely because their identities are fused to the project. Declared victims derive near the full-target end: liturgical communities bear the entire symbolic extraction with zero offsetting benefit, and their exit is identity-locked — the practice is who they are. Partial_transmission_households are genuinely dual-positioned: they pay the labor now while their children receive the asset, so the structural derivation from the victim declaration alone would overshoot toward full target. An explicit override (power_atom moderate, d_value 0.45) corrects this; moderate is otherwise unused in this story, so the override lands on exactly one seat. Observers hold analytical seats and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whether a heritage language can regain vernacular life, and on what evidence — is live, so no mandatrophy resolution is declared and no sunset clause is authored. The tangled_rope classification earns its keep by blocking both mislabels. Reading the arrangement as a pure snare would erase the genuine coordination achievement: in polities where the infrastructure succeeded, actual native-speaker generations exist who would not otherwise, and that is not cover — it is the delivered good. Reading it as a pure rope would erase the documented asymmetry: the same structure that produces speakers strips standing from communities who never consented to the test and channels careers and budgets to its administrators. The drift risk to watch is decay toward inert theatricality: if the native/non-native binary collapses empirically (see omega native_speaker_binary_validity), certification could survive as performance — counting heritage speakers as native to preserve the metric — which is the signature the theater_ratio series is positioned to catch; notably, theater eases slightly after its 2000 peak rather than ratcheting, consistent with a function still substantially alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the living_language_status kernel should govern vitality classification — native generational transmission (this file), liturgical preservation, or literary continuity?',
    'Not resolvable by data alone: the choice turns on what ''living'' is for — demographic continuity, ritual continuity, or cultural production. It is settled politically and axiologically per polity; cross-file comparison of the three sibling stories locates the structural stakes of each choice.',
    'If liturgical_preservation_reading governed, the beneficiary/victim structure inverts: liturgical communities become custodian-beneficiaries and the revival apparatus loses its gatekeeping warrant; if literary_continuity_reading governed, native-speaker counts lose gatekeeping power, diaspora literary production counts fully, and this reading''s extraction asymmetry collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: this constraint is one of three competing readings of one kernel; the sibling readings would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    native_speaker_binary_validity,
    'Is the categorical native/non-native speaker distinction empirically robust, or do new-speaker continua — semi-speakers, latent bilinguals, L1 attrition — dissolve the criterion''s operational core?',
    'Longitudinal acquisition studies comparing home-transmitted children, intensive-school second-language acquirers, and adult learners on the outcomes the criterion actually cares about: probability of intergenerational restart and incidence of unplanned vernacular use.',
    'If the binary fails, the criterion loses its empirical warrant and certification drifts toward theatrical counting, decaying the arrangement toward inertia; if it holds, the necessity claim stands and the moderate extraction reflects real transmission costs rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_binary_validity, empirical, 'Empirical soundness of the native/non-native binary underpinning the criterion.').

omega_variable(
    internalized_vitality_verdict,
    'Is the burden on liturgical-only communities purely external — classification, funding gates, public framing — or do younger members internalize the ''dead language'' verdict and disengage from transmission?',
    'Cohort studies of engagement and attrition in liturgical communities before and after public vitality-classification events, comparing communities exposed to revival-state rhetoric with geographically or socially insulated counterparts.',
    'An internalized component raises effective suppression above the structural measure and makes it persist after policy reform; a purely external profile means removing the classification removes the harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vitality_verdict, empirical, 'Structural versus internalized component of the suppression borne by liturgical communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1880, living_language_status__native_generation_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(livi_tr_t1910, living_language_status__native_generation_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement(livi_tr_t1940, living_language_status__native_generation_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(livi_tr_t1970, living_language_status__native_generation_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(livi_tr_t2000, living_language_status__native_generation_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(livi_tr_t2025, living_language_status__native_generation_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(livi_be_t1880, living_language_status__native_generation_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(livi_be_t1910, living_language_status__native_generation_reading, base_extractiveness, 1910, 0.45).
narrative_ontology:measurement(livi_be_t1940, living_language_status__native_generation_reading, base_extractiveness, 1940, 0.55).
narrative_ontology:measurement(livi_be_t1970, living_language_status__native_generation_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(livi_be_t2000, living_language_status__native_generation_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(livi_be_t2025, living_language_status__native_generation_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1880, living_language_status__native_generation_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(livi_su_t1910, living_language_status__native_generation_reading, suppression_requirement, 1910, 0.35).
narrative_ontology:measurement(livi_su_t1940, living_language_status__native_generation_reading, suppression_requirement, 1940, 0.55).
narrative_ontology:measurement(livi_su_t1970, living_language_status__native_generation_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(livi_su_t2000, living_language_status__native_generation_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(livi_su_t2025, living_language_status__native_generation_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'is the language living?' decomposes into three structurally distinct claims with different epsilon values and different party structures, per the epsilon-invariance principle: this file (native generational transmission; moderate extraction; nationalist and institutional beneficiaries, liturgical and partial-transmission victims), liturgical_preservation_reading (ritual continuity suffices; custodial religious institutions as beneficiaries), and literary_continuity_reading (productive literary medium suffices; literati and publishers as beneficiaries). Each is authored as its own constraint file with its own stable epsilon, its own stakeholders, and its own claimed type; they are linked here as a constraint family. Where the native-generation reading captured state machinery it changed the resource environment of the other two, but the edges recorded here assert family membership, not causal priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
