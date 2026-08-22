% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Reading of Kami-Buddha Simultaneous Veneration (shinbutsu-shugo)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the domain-partition reading of the
 *   shinbutsu-shugo (kami-buddha combinatory) kernel: the widespread
 *   pre-Meiji Japanese practice of simultaneously venerating kami and
 *   buddhas, expressed institutionally through jingu-ji (combined
 *   shrine-temple complexes) and functionally through the division between
 *   kami's this-worldly efficacy (harvest, fertility, protection, prosperity)
 *   and buddhas' soteriological efficacy (post-mortem salvation, ancestral
 *   memorialization). Under this reading, the two address-systems are
 *   functionally distinct, not competing or fused, and simultaneous
 *   veneration reflects domain-appropriate specialization rather than
 *   confusion, syncretic blending, or unresolved contradiction. This is one
 *   of three readings of the same kernel; the sibling readings
 *   (ontological_fusion_reading, treating kami and buddhas as identical
 *   beings under honji-suijaku metaphysics, and
 *   pragmatic_incoherence_reading, treating the practice as sustained
 *   contradiction absent enforcement pressure) are separate constraint
 *   stories with their own ε values, per the ε-invariance principle — this
 *   story does not average over them or hedge toward them.
 *
 * KEY AGENTS:
 *   - lay_households_seeking_prosperity: primary beneficiary (moderate/mobile) — addresses domain-appropriate needs to each tradition
 *   - temple_shrine_ritual_specialists: joint agenda-setters and beneficiaries (organized/constrained) — administer separate but coordinated domains
 *   - village_communities: coordination beneficiary (moderate/mobile) — organizes communal ritual life across both domains
 *   - doctrinal_purists_excluded_from_practice: excluded voice (powerless/constrained) — objects on grounds the domains are not truly separable
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — evaluates the reading against sibling accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Reading of Kami-Buddha Simultaneous Veneration (shinbutsu-shugo)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'd6e338a7-93ca-45b8-af21-14408ed8fb83').
narrative_ontology:cs_kernel_codification('d6e338a7-93ca-45b8-af21-14408ed8fb83', distributed).
narrative_ontology:cs_authority_grounding('d6e338a7-93ca-45b8-af21-14408ed8fb83', practice).
narrative_ontology:cs_interpretation_layer_present('d6e338a7-93ca-45b8-af21-14408ed8fb83').
narrative_ontology:cs_reading_relation('d6e338a7-93ca-45b8-af21-14408ed8fb83', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6e338a7-93ca-45b8-af21-14408ed8fb83', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('d6e338a7-93ca-45b8-af21-14408ed8fb83', foundational, kami_and_buddhas_are_functionally_distinct_domain_specialists).
narrative_ontology:cs_axiom_status(kami_and_buddhas_are_functionally_distinct_domain_specialists, holdable).
narrative_ontology:cs_axiom_grounding('d6e338a7-93ca-45b8-af21-14408ed8fb83', kami_and_buddhas_are_functionally_distinct_domain_specialists, conventional).
narrative_ontology:cs_axiom('d6e338a7-93ca-45b8-af21-14408ed8fb83', secondary, simultaneous_veneration_reflects_appropriate_specialization_not_contradiction).
narrative_ontology:cs_axiom_status(simultaneous_veneration_reflects_appropriate_specialization_not_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('d6e338a7-93ca-45b8-af21-14408ed8fb83', simultaneous_veneration_reflects_appropriate_specialization_not_contradiction, instrumental).
narrative_ontology:cs_reference_frame('d6e338a7-93ca-45b8-af21-14408ed8fb83', heian_era_combinatory_practice_norm).
narrative_ontology:cs_drift_state('d6e338a7-93ca-45b8-af21-14408ed8fb83', late_edo_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d6e338a7-93ca-45b8-af21-14408ed8fb83', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_households_seeking_prosperity).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, temple_shrine_ritual_specialists).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, village_communities).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_domain_specialization_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, kami_this_worldly_efficacy_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, buddha_soteriological_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition kami at shrines for harvest, fertility, health, and protection from calamity in daily life, while separately arranging Buddhist funerary and memorial rites for deceased kin through temples. Under this reading they are not hedging or confused; they are addressing two functionally distinct address-points, each competent in its own domain, and could in principle draw on either tradition alone if their needs were purely one domain or the other.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_households_seeking_prosperity, beneficiary,
    moderate, biographical, mobile, regional).

% Shrine priests (kannushi) and temple clergy (so) each administer and interpret their own domain's rites, jointly co-locating at shrine-temple complexes (jingu-ji) that formalize the division of labor. They do not compete for the same petitions because the petitions are understood to be domain-distinct; each specialist's authority rests on competence in a bounded domain rather than displacement of the other.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, temple_shrine_ritual_specialists, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, temple_shrine_ritual_specialists, agenda_setter).

% Organize communal life around both shrine festivals (matsuri, tied to agricultural cycles and this-worldly welfare) and temple-based ancestral rites (tied to death and continuity across generations). The two calendars and institutions coordinate rather than compete, giving the community a complete ritual repertoire across the domains that matter to it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, village_communities, beneficiary,
    moderate, generational, mobile, local).

% Sectarian reformers who hold that kami worship and Buddhist practice are not merely different domains but different truth-claims that cannot both be correct — they object to domain-partition on doctrinal grounds but have limited institutional voice against a widely practiced synthesis backed by both shrine and temple establishments.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, doctrinal_purists_excluded_from_practice, excluded,
    powerless, biographical, constrained, regional).

% Analyze shinbutsu-shugo as a historical case of religious syncretism, comparing the domain-partition account against ontological-fusion (honji suijaku) and incoherence accounts of the same practice complex, without themselves being petitioners in either domain.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(simultaneous_veneration__domain_partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides the total space of human religious need into two functionally complementary domains — this-worldly prosperity/protection (kami) and post-mortem salvation/liberation (buddhas) — so that practitioners can address whichever need is live without requiring either tradition to claim total competence it does not have.
% TRANSFER_FUNCTION: No systematic transfer of resources from one party to another; ritual fees and offerings flow from petitioners to whichever specialist administers the relevant domain, roughly in proportion to services actually sought (harvest blessing vs. funerary rite), not extracted through coercion or bundling.
% ABSENT_VOICES: Doctrinal purists on both the Buddhist and kami-cultic sides who hold their tradition should have exclusive or superior competence across all domains are marginalized by the popularity and institutional entrenchment of the partition arrangement; they surface later as Meiji-era shinbutsu bunri reformers.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, practitioners would either have to choose one tradition as exclusively authoritative for all needs (collapsing the jingu-ji co-location system) or adopt a different account of why both are addressed simultaneously (fusion or incoherence) — either way, the institutional co-location of shrines and temples and the division of ritual labor between specialists would need to reorganize.
% FOUNDING_PROBLEM: Neither kami cults nor imported Buddhism alone offered a complete account of both immediate this-worldly welfare and post-mortem fate; communities needed both kinds of address and needed a way to hold both traditions as legitimate without contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion (outside both shrine and temple institutions) document jingu-ji co-location and functional division of ritual labor across centuries as corroborating the domain-partition account; but the same historians also document Meiji-era shinbutsu bunri polemicists and some modern scholars arguing the partition was a retrospectively tidy gloss over what was actually looser and more contradictory practice — the founding-problem account is attested from outside the beneficiary set but not uncontested even there.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because under the domain-partition reading no party captures rents from the arrangement — practitioners pay for services roughly proportional to what they seek, and no institutional actor extracts by claiming false competence over the other's domain. Suppression is low (0.08) because nothing in this reading requires coercing anyone into holding both traditions as legitimate; the co-location and division of labor emerged through practical adequacy, not enforcement. Theater ratio is low but drifts slightly upward (0.10 to 0.15) reflecting the gradual institutionalization of jingu-ji complexes into somewhat more formal, occasionally more ceremonial arrangements over the centuries, without this becoming a dominant feature. Accessibility collapse is moderate (0.25): once a household accepts the domain-partition framing, switching to a purely single-tradition account becomes somewhat less intuitive, but the alternative framings (fusion, incoherence) remained live and did not require suppression to persist. Resistance is low (0.10), consistent with a genuinely low-conflict coordination arrangement rather than one requiring active defense.
 *
 * PERSPECTIVAL GAP:
 *   Lay households and village communities experience this as straightforward complementary service provision — two specialists, two domains, no tension. Doctrinal purists, largely excluded from the practical arrangement's institutional voice, would compute this constraint quite differently, as a compromise that obscures a real ontological or truth-claim conflict; their exclusion from the ritual-specialist power structure is why their objection does not register as active resistance in the metrics despite being a genuine dissenting position.
 *
 * DIRECTIONALITY LOGIC:
 *   Both kami-domain and buddha-domain specialists are declared beneficiaries because each administers a bounded, functioning domain without needing to displace the other — this is the structural signature of genuine complementary coordination rather than one party subsidizing another. No victims are declared under this reading because no party's domain is treated as illegitimate or absorbed; petitioners pay for services they seek in each domain, which the engine's beneficiary-weighted directionality derivation should register as low-to-symmetric d across all named parties, consistent with a Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain-partition reading resists mandatrophy mislabeling in both directions: it does not let genuine functional specialization get flattened into 'pure extraction' (there is no victim class whose costs are hidden behind a coordination story), nor does it let a possibly-contested syncretic practice get certified as eternally settled coordination — the founding_problem_status is authored as contested precisely because Meiji-era shinbutsu bunri reformers and some purist voices dispute whether the domains were ever as cleanly separable as this reading holds. The mandatrophy question here is not 'has this arrangement's function died,' but 'was the coordination-function characterization ever fully accurate' — which is the proper subject of the omega variables and the sibling readings, not of this reading's own metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_stability,
    'Were the this-worldly/afterlife domain boundaries between kami and buddha veneration actually stable and consistently understood across regions and centuries, or did they shift and blur in ways that undermine a clean partition account?',
    'Comparative textual and ritual-record analysis across regions and periods (Heian, Kamakura, Muromachi, Edo) to check whether the this-worldly/salvific division was consistently drawn, or whether kami were also invoked for post-mortem concerns and buddhas for this-worldly benefits (genze riyaku) in ways that cut against strict partition.',
    'If the boundary was consistently stable, the domain-partition reading is well-supported as the dominant lived understanding; if it was routinely blurred (kami invoked for afterlife matters, buddhas invoked for harvest and healing), the partition account may be a retrospective simplification and the pragmatic_incoherence_reading gains support instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_stability, empirical, 'Whether the this-worldly/afterlife domain division was empirically stable across shinbutsu-shugo''s history.').

omega_variable(
    practitioner_self_understanding_vs_scholarly_reconstruction,
    'Did ordinary lay practitioners themselves hold an explicit domain-partition theory, or is this reading a scholarly reconstruction imposed retrospectively on practice that practitioners experienced without articulated theoretical justification?',
    'Examination of lay diaries, votive records, and popular religious tracts (as opposed to elite doctrinal writings from jingu-ji clergy) for evidence of explicit domain-reasoning versus unreflective habitual practice.',
    'If lay practitioners held no explicit theory at all, the domain-partition reading may still be structurally accurate as a description of functional practice, but its status as a ''reading'' practitioners themselves held (versus one scholars construct to make sense of otherwise unreflective practice) would need qualification — this bears on whether the arrangement is best modeled as a Rope (endorsed coordination) or something closer to unarticulated habit that any of the three readings could describe post hoc.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_self_understanding_vs_scholarly_reconstruction, conceptual, 'Whether domain-partition was an articulated practitioner theory or a scholarly reconstruction of unreflective practice.').

omega_variable(
    meiji_separation_as_evidence,
    'Does the relative ease with which Meiji-era shinbutsu bunri (forced separation) was imposed indicate the underlying practice was more fragile/incoherent than the domain-partition reading suggests, or does it simply reflect an unrelated exercise of state power against a genuinely stable and functional prior arrangement?',
    'Historical analysis of the actual mechanisms and resistance (or lack thereof) during Meiji-era temple-shrine separation and the subsequent haibutsu kishaku (anti-Buddhist) movement, comparing regions and social classes.',
    'If separation met widespread grassroots resistance from communities defending the combined arrangement, this supports the domain-partition (or fusion) reading as describing a genuinely valued, coherent system; if separation proceeded with comparatively little popular resistance, this lends some support to the pragmatic_incoherence_reading''s claim that the arrangement was sustained mainly by absence of enforcement pressure rather than positive coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence, empirical, 'Whether Meiji-era forced separation''s reception indicates prior coherence or prior fragility of the combinatory arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.13).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.14).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.14).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.15).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.1).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.11).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.11).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.12).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'shinbutsu-shugo / simultaneous veneration' kernel per the ε-invariance principle. domain_partition_reading (this story) authors ε=0.12, a Rope classification, and no victims, describing functionally complementary domains. ontological_fusion_reading authors a metaphysical-identity claim (honji-suijaku) with its own independent ε and stakeholder structure. pragmatic_incoherence_reading authors the practice as sustained contradiction, likely with a different extractiveness/suppression profile reflecting the absence of genuine resolution. All three share the same underlying historical practice complex but are structurally distinct claims about what that practice IS, and must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
