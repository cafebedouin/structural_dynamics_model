% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Continuity Reading of Orthographic Legitimacy (Script-Tradition Access)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTINUITY reading of the orthographic
 *   legitimacy kernel: legitimacy is judged by whether the population retains
 *   living access to the historical, religious, and literary corpus that a
 *   prior script encoded. Under this reading, the 1928 Turkish alphabet
 *   reform (Arabic to Latin script) is evaluated purely on the axis of
 *   textual continuity, not literacy uplift (instrumentalist reading) or
 *   civilizational realignment (modernist reading). The story treats script
 *   incompatibility as close to a physical fact — a population trained in one
 *   symbol system cannot fluently read another without dedicated retraining,
 *   a near-mountain constraint on human cognition and pedagogy — while
 *   treating the POLICY DECISION to break continuity as the extractive event
 *   the reading tracks. ε is authored low (0.28) because the continuity
 *   criterion itself is not an extraction mechanism; what it registers is
 *   loss, layered on top of a near-natural literacy-transfer barrier. This is
 *   deliberately NOT the same constraint as the instrumentalist or modernist
 *   readings — those would author different ε, different beneficiaries, and
 *   different victims, because they are about different structural claims
 *   riding the same kernel event.
 *
 * KEY AGENTS:
 *   - religious_scholarly_class: beneficiary of continued script fluency, organized/identity_locked
 *   - ottoman_era_literate_elite_descendants: beneficiary via inherited family literacy, moderate/mobile
 *   - post_reform_generations: primary victim, powerless/trapped, severed from pre-1928 corpus
 *   - vernacular_arabic_script_readers: mirrored victim, powerless/trapped, severed from post-reform civic life
 *   - state_orthographic_authority: agenda_setter, institutional/arbitrage, judged here by continuity legitimacy alone
 *   - manuscript_and_archive_custodians: analytical observer of the widening access gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.28).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Continuity Reading of Orthographic Legitimacy (Script-Tradition Access)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '922d5947-3b8f-4acc-b50f-29974d7d645c').
narrative_ontology:cs_kernel_codification('922d5947-3b8f-4acc-b50f-29974d7d645c', distributed).
narrative_ontology:cs_authority_grounding('922d5947-3b8f-4acc-b50f-29974d7d645c', lineage).
narrative_ontology:cs_interpretation_layer_present('922d5947-3b8f-4acc-b50f-29974d7d645c').
narrative_ontology:cs_reading_relation('922d5947-3b8f-4acc-b50f-29974d7d645c', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('922d5947-3b8f-4acc-b50f-29974d7d645c', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('922d5947-3b8f-4acc-b50f-29974d7d645c', foundational, textual_tradition_access_grounds_legitimacy).
narrative_ontology:cs_axiom_status(textual_tradition_access_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('922d5947-3b8f-4acc-b50f-29974d7d645c', textual_tradition_access_grounds_legitimacy, deontological).
narrative_ontology:cs_axiom('922d5947-3b8f-4acc-b50f-29974d7d645c', secondary, script_rupture_constitutes_irreversible_generational_loss).
narrative_ontology:cs_axiom_status(script_rupture_constitutes_irreversible_generational_loss, holdable).
narrative_ontology:cs_axiom_grounding('922d5947-3b8f-4acc-b50f-29974d7d645c', script_rupture_constitutes_irreversible_generational_loss, empirically_contingent).
narrative_ontology:cs_reference_frame('922d5947-3b8f-4acc-b50f-29974d7d645c', pre_reform_textual_continuity_norm).
narrative_ontology:cs_drift_state('922d5947-3b8f-4acc-b50f-29974d7d645c', post_1928_reform_contemporary, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('922d5947-3b8f-4acc-b50f-29974d7d645c', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_scholarly_class).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, ottoman_era_literate_elite_descendants).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, vernacular_arabic_script_readers).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, script_continuity_preserves_textual_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains trained fluency in Arabic script and the corpus it carries (Quranic exegesis, Ottoman legal and literary archives). Their authority and vocation depend on this script remaining the operative orthography or at least remaining transmissible; a script break threatens to make their expertise a specialist antiquarian skill rather than a living civic function.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_scholarly_class, beneficiary,
    organized, civilizational, identity_locked, national).

% Family archives, land deeds, correspondence, and literary manuscripts in Arabic script remain directly legible to them or accessible through inherited family literacy. They benefit from any orthographic policy that keeps this script alive as a readable, not merely archival, form.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottoman_era_literate_elite_descendants, beneficiary,
    moderate, generational, mobile, national).

% Educated exclusively in the new script, unable to read pre-reform gravestones, family papers, religious texts, or the bulk of the Ottoman-era literary and administrative corpus without specialist training. They did not choose the reform and cannot retroactively acquire the severed literacy without significant independent investment; the loss is generational and largely irreversible for them as a cohort.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, generational, trapped, national).

% Older cohort literate only in the pre-reform script at the time of transition, now functionally illiterate in the state's operative orthography. Their access to civic life, print media, and administration is degraded even though their access to tradition is intact; they bear a mirrored cost to the post-reform generations.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, vernacular_arabic_script_readers, payer,
    powerless, biographical, trapped, regional).

% Sets and enforces the operative script through education, print, and administration. Under the continuity reading, its legitimacy is judged by whether it preserves the population's living access to the textual tradition, not by literacy statistics or geopolitical alignment; from this reading's vantage the 1928 reform is a rupture the state imposed against this legitimacy criterion.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, state_orthographic_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Libraries, archives, and religious endowment institutions holding the pre-reform textual corpus. They observe firsthand the widening gap between the surviving textual record and the population's capacity to read it, without themselves setting orthographic policy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, manuscript_and_archive_custodians, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared, stable orthography lets successive generations read the same religious, legal, literary, and epigraphic record without specialist retraining — the coordination problem is intergenerational textual access, solved by NOT changing the symbol system out from under the population.
% TRANSFER_FUNCTION: When the orthography is changed, the arrangement transfers effortless textual access away from everyone who has not yet been trained in the new system and toward whichever cohort (old-script literates, professional Arabists, religious specialists) retains or can afford to retain the old-script skill; nothing material is transferred under the continuity reading when orthography holds steady — the reading is stated for the counterfactual of change.
% ABSENT_VOICES: The generations born after a script change who never got a vote on the reform are structurally absent from the decision that severed them from their own documentary and religious heritage; their objection is registered only retrospectively, through the archives they cannot read.
% DISAPPEARANCE_RATIONALE: If the continuity criterion for legitimacy disappeared entirely, the state's freedom to change scripts would be unconstrained by any tradition-access concern, and instrumentalist or modernist criteria would govern alone. The religious-scholarly class and archive custodians would say the world rearranges catastrophically (textual civilization severed from its base); the state and instrumentalist reformers would say the world stays functionally unchanged (literacy and administration were never bottlenecked on script continuity). The dispute is exactly the kernel contest this story is one reading of.
% FOUNDING_PROBLEM: Long-lived textual traditions (religious scripture, legal precedent, family and land records, literary canon) require a stable symbol system across centuries; the continuity reading names orthographic stability as what solves this, and treats any reform as incurring a real, possibly irreversible access cost.
% FOUNDING_PROBLEM_CORROBORATION: Independent philologists and historians studying post-1928 Turkish document access confirm the empirical severance (most Turkish citizens today cannot read Ottoman-script primary sources without training) from outside both the religious-scholarly beneficiary class and the state apparatus; UNESCO-adjacent heritage-access literature corroborates the general pattern of script reform producing durable intergenerational literacy gaps to historical corpora.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28) and rises only slowly over the interval: the continuity reading does not describe an ongoing rent-collection mechanism so much as a one-time historical rupture whose costs compound gradually as fewer living people retain bridging literacy. Suppression is authored moderate-high (0.62) because the state's educational and print apparatus actively reproduces the new orthography and does not offer old-script literacy as a live option to new generations — the suppression is of the ALTERNATIVE (dual literacy), not of persons. Accessibility collapse is authored very high (0.88): once the reform is decades old, the population's practical alternatives for reading the pre-reform corpus have nearly vanished, which is exactly the near-mountain character of script-incompatibility. Resistance is authored low-moderate (0.35): there is real but diffuse resistance (religious and nationalist-conservative constituencies periodically raise the issue) but no organized mass movement to reverse the script.
 *
 * PERSPECTIVAL GAP:
 *   From the religious scholarly class's seat, the continuity criterion is close to a mountain: script stability is simply what preserving a textual civilization requires, an almost logical necessity given how symbol systems work. From the post-reform generations' seat, the same criterion computes as evidence of a real, uncompensated loss imposed on them by a decision made before they existed. The state's seat treats the criterion as one input among several it is not obligated to weight highest — which is exactly why this kernel needs three separate reading-stories rather than one constraint with a measurement parameter.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those who retain or inherited old-script fluency (religious scholars, families with surviving literate lineage) — the constraint (continuity-as-legitimacy-criterion) validates their existing capital and does not extract from them, so their derived d sits low. Victims are the post-reform generations and the residual old-script-only cohort, both powerless and trapped with respect to this specific access loss — no feasible individual exit restores the severed literacy, so derived d sits high for both. Note the mirror-image victim structure: post-reform generations lost the OLD text; old-script-only elders lost operative access to the NEW civic sphere. The continuity reading centers the former loss as its object.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resists mandatrophy mislabeling in a specific way: it does not claim the pre-reform script SHOULD be restored (that would be a policy preference, not a structural claim), nor does it claim the reform was illegitimate under every criterion. It isolates one legitimacy axis — tradition-access — and reports that axis honestly as substantially unmet by the post-1928 arrangement, without adjudicating whether other axes (literacy, modernization) outweigh it. This keeps the reading from being either a pure nostalgia complaint or a disguised restoration mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_reading_kernel_identity,
    'Is the continuity reading of orthographic legitimacy a distinct structural claim from the instrumentalist and modernist readings, or are all three merely different rhetorical framings of a single historical decision with one true extraction profile?',
    'Compare the beneficiary/victim sets and ε values authored across the three sibling reading-stories; if they diverge substantially (as expected structural deltas indicate), the readings are structurally distinct constraints sharing a kernel event, not one constraint under three descriptions.',
    'If the readings collapse to one true profile, the kernel-reading framework would be over-decomposing a single constraint; if they remain genuinely distinct (different ε, different victims), each reading''s classification stands independently and none can be used to adjudicate the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_reading_kernel_identity, conceptual, 'Whether the three kernel readings are genuinely distinct constraints or one constraint under three rhetorical framings.').

omega_variable(
    mountain_vs_constructed_severance,
    'Is the script-incompatibility barrier (high accessibility_collapse) a near-physical fact of human cognitive literacy transfer, or is it substantially a constructed outcome of the specific pedagogical and archival choices the state made after 1928 (e.g., choosing not to teach old-script literacy as a secondary skill, not digitizing/transliterating the archive at scale)?',
    'Examine comparative cases where post-reform states DID maintain dual-script literacy programs or large-scale transliteration efforts, and compare resulting population-level access rates to the Turkish case.',
    'If the barrier is substantially constructed rather than natural, the beneficiaries (religious scholarly class, archive custodians) may have declared beneficiaries on what is closer to an FSM candidate — a claimed-mountain-like barrier that in fact reflects policy choices benefiting specific actors'' authority over textual gatekeeping — rather than a genuine cognitive-literacy mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_severance, empirical, 'Whether script incompatibility is closer to natural cognitive fact or constructed policy outcome, bearing on FSM candidacy.').

omega_variable(
    victim_coalition_potential,
    'Could post-reform generations and vernacular Arabic-script readers, despite being on opposite sides of the literacy transition, form a coalition around demanding dual-literacy education or expanded transliteration/archival access, given both are powerless individually?',
    'Track whether civil-society organizations (heritage preservation groups, religious education advocates) have organized cross-generational coalitions around expanded access to Ottoman-script materials in schools or public archives.',
    'A successful coalition would shift the powerless/trapped victim seats toward organized/constrained, changing the derived directionality and potentially the classification toward a contested rope if continuity access is restored as a public good.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_potential, empirical, 'Whether powerless victim groups on opposite sides of the reform could coalesce around shared access demands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1928, 0.05).
narrative_ontology:measurement(orth_tr_t1948, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(orth_tr_t1968, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(orth_tr_t1988, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement(orth_tr_t2008, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement(orth_tr_t2024, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1928, 0.05).
narrative_ontology:measurement(orth_be_t1948, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(orth_be_t1968, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(orth_be_t1988, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1988, 0.24).
narrative_ontology:measurement(orth_be_t2008, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2008, 0.26).
narrative_ontology:measurement(orth_be_t2024, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(orthographic_legitimacy_kernel__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the orthographic_legitimacy_kernel, each authored as a separate constraint file per the ε-invariance principle. The continuity reading authors low-moderate ε and near-mountain accessibility_collapse, centered on textual-tradition loss. The instrumentalist reading is expected to author higher beneficiary breadth (literacy gains distributed across the population) and likely computes closer to a genuine rope. The modernist reading is expected to center a civilizational-realignment beneficiary/victim structure with its own distinct ε, likely more contested and extractive given the ideological stakes of the Ottoman/Islamic-past rupture claim. All three share the 1928 Turkish alphabet reform as their common historical referent but are NOT the same constraint — changing which legitimacy criterion is applied changes the beneficiary set, the victim set, and the extraction profile, which is precisely the signal that these are three constraints under one colloquial kernel label rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
