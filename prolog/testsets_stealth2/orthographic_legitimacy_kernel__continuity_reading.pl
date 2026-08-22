% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Legitimacy — Continuity Reading (Access to the Inherited Corpus)
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the
 *   orthographic_legitimacy_kernel: the claim that a writing system is
 *   legitimate insofar as it preserves the community's unmediated access to
 *   its inherited religious, historical, and literary corpus. The standing
 *   arrangement under contest is the post-1928 Latin-letter orthographic
 *   order in Turkey, which replaced Arabic-script Ottoman Turkish by statute,
 *   barred the old script from official use, and educated every subsequent
 *   generation exclusively in the new alphabet. Assessed by this reading's
 *   own lights, the arrangement's defining fact is severance: a seven-century
 *   textual inheritance — administrative records, endowment deeds, divan
 *   poetry, religious commentary, family papers — sits behind a script wall
 *   the majority cannot cross without deliberate, costly second-literacy
 *   acquisition. The reading's grievance is real, but its structure is loss
 *   without a collector: no identifiable party gathers income, authority, or
 *   resources from the barrier's persistence at any meaningful scale, the
 *   small scarcity rents of residual tradition-bearers are eroding and held
 *   mostly by advocates of diffusion, and nothing enforces the barrier today
 *   — it self-maintains as a competence fact laid down by a 1928 decision.
 *   The claimed type is therefore piton (a former coordination standard whose
 *   function transferred to the Latin regime and whose residue persists by
 *   inertia, administered by a state that could fund remediation but bears
 *   almost none of the access cost), not mountain (the barrier is
 *   forward-dissolvable at finite cost, was choice-activated, and meets
 *   active remediation resistance) and not snare (there is no capturer).
 *   Metrics are authored independently of that claim from the arrangement's
 *   actual operation; if the engine computes otherwise, the divergence is the
 *   datum. KEY AGENTS are enumerated by structural relationship in
 *   key_agents.
 *
 * KEY AGENTS:
 *   - turkish_republican_state: Agenda-setter (institutional/arbitrage) — authored the 1928 script change; administers the education system that could fund remediation; bears almost none of the access cost
 *   - post_reform_generations: Primary bearer of the loss (powerless/constrained) — severed from the pre-1928 corpus; individually escapable at real cost, collectively unprovided-for
 *   - residual_tradition_bearers: Marginal beneficiary (moderate/identity_locked) — scarce fluency confers interpretive authority; rents eroding; mostly campaign for their own scarcity's abolition
 *   - religious_conservative_constituency: Organized claimant (organized/identity_locked) — bears the severance most acutely in worship and lineage while drawing political identity from the grievance
 *   - pre_reform_literate_community: Excluded historical party (organized/trapped) — never consulted at the decision point; institutionally dissolved within a decade
 *   - comparative_script_reform_scholarship: Analytical observer (analytical/analytical) — external, cross-national attestation of the access gap's size and causes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.2).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.15).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, piton).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy — Continuity Reading (Access to the Inherited Corpus)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political linguistics / state formation / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '3aa21b65-c4e2-45f1-ae19-572644e4f5b8').
narrative_ontology:cs_kernel_codification('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', distributed).
narrative_ontology:cs_authority_grounding('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', lineage).
narrative_ontology:cs_interpretation_layer_present('3aa21b65-c4e2-45f1-ae19-572644e4f5b8').
narrative_ontology:cs_reading_relation('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', foundational, legitimacy_requires_corpus_access_continuity).
narrative_ontology:cs_axiom_status(legitimacy_requires_corpus_access_continuity, holdable).
narrative_ontology:cs_axiom_grounding('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', legitimacy_requires_corpus_access_continuity, deontological).
narrative_ontology:cs_axiom('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', secondary, script_reform_incurs_transmission_debt).
narrative_ontology:cs_axiom_status(script_reform_incurs_transmission_debt, holdable).
narrative_ontology:cs_axiom_grounding('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', script_reform_incurs_transmission_debt, deontological).
narrative_ontology:cs_reference_frame('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', continuous_script_transmission_order).
narrative_ontology:cs_drift_state('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', post_1928_latin_regime, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3aa21b65-c4e2-45f1-ae19-572644e4f5b8', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, residual_tradition_bearers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_conservative_constituency).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, religious_conservative_constituency).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, script_choice_determines_heritage_access).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, transmission_discontinuity_irreversibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carried out the 1928 replacement of the Arabic-script Ottoman writing system with a Latin-letter alphabet, made the old script unusable in official documents, and has administered public education ever since. It funds token Ottoman-Turkish electives and occasionally stages Ottoman heritage ceremonially, but has never invested in mass dual-script literacy; the cost of building a teaching corps for the old script falls on it, while the daily burden of the access gap falls almost entirely on others. It can rebrand the Ottoman past aesthetically at will while the population remains unable to read it.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, turkish_republican_state, agenda_setter,
    institutional, generational, arbitrage, national).

% A small and shrinking class — Ottomanist philologists, archivists, calligraphers holding icazet diplomas, clergy formed in pre-reform or traditionalist curricula — who can read the pre-1928 corpus directly. Their fluency is scarce enough to confer interpretive authority and paid mediation work (transcription, authentication, expert testimony), though the gains are modest and shrink as each cohort dies and as machine transcription improves. Most of them campaign for the skill to be taught widely, which would erase their own scarcity.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, residual_tradition_bearers, beneficiary,
    moderate, biographical, identity_locked, national).

% Everyone schooled after the alphabet change inherits a century of administrative records, endowed-property deeds, family correspondence, gravestones, and a large religious and literary canon written in a script they were never taught. Reading any of it requires months of deliberate study, enrollment in scarce university programs, or paying an intermediary. Individually each person can acquire the skill; collectively no institution provides it by default, and the cut-off deepens with each graduating class.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, generational, constrained, national).

% Religious communities, Ottoman-nostalgic movements, and heritage foundations feel the severance most sharply where it touches worship and lineage: Qur'an commentary traditions, endowment charters, genealogical papers. The same loss supplies their political identity — the grievance organizes parties, foundations, and curriculum projects — so they simultaneously suffer the cut-off and draw mobilization strength from its continuation, while demanding its reversal.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_conservative_constituency, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__continuity_reading, religious_conservative_constituency, beneficiary).

% The scribes, ulema, poets, calligraphers, and ordinary lettered subjects whose working literacy the 1928 change rendered obsolete. They were never consulted when the change was decided; dissenting deputies and religious authorities objected and were overridden, and within a decade the community's institutional carriers (medreses, scribal offices) were closed or absorbed. Almost none survive today; their objection survives only in the archive and in the tradition they were the last to read natively.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, pre_reform_literate_community, excluded,
    organized, generational, trapped, national).

% Historians, sociolinguists, and archival scientists in Turkey and abroad who study alphabet reforms comparatively — Turkey 1928 alongside Iran's retention of the Perso-Arabic script, the Soviet script switches in Central Asia, Chinese character simplification. They document what continuity and rupture each cost in measurable access, and they attest the size of the Turkish access gap from outside any domestic faction.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, comparative_script_reform_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded community of shared textual competence — master-apprentice calligraphic transmission chains, clerical curricula, scholarly lineages — inside which the pre-1928 corpus remains readable and authoritative; the surrounding Latin-letter order separately coordinates mass literacy, but that is the sibling readings' account, not this one.
% TRANSFER_FUNCTION: Moves interpretive authority over the inherited corpus from the general post-reform public to the small class retaining old-script competence, and moves the corpus itself out of common circulation into expert-mediated custody; the underlying access loss moves in the opposite direction, onto every subsequent school cohort.
% ABSENT_VOICES: The pre-reform literate community — scribes, ulema, poets, calligraphers — whose working world the reform dismantled, is no longer alive to speak; at the moment of decision in 1928 the mass of Arabic-literate subjects were never consulted; today the majority of post-reform Turks have no seat in debates conducted among state planners, revival politicians, and academic specialists.
% DISAPPEARANCE_RATIONALE: If the barrier dissolved overnight — universal dual-script competence granted at once — millions of archival records, endowment deeds, gravestones, and literary texts would re-enter circulation; religious and scholarly authority structures that rest on mediated access would shift; the historiography of the Ottoman period would democratize beyond the specialist guild; and the continuity constituency's central grievance would dissolve, reorganizing its politics around something else.
% FOUNDING_PROBLEM: Preserving the community's access to its own inherited corpus — scripture, law, poetry, administration — across generations, within a single continuous script.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by comparative script-reform scholarship documenting the access consequences of the Turkish cut-off against continuity-retaining cases, by archival-science literature on unprocessed Ottoman-era holdings, and by international manuscript-preservation bodies; the Turkish state itself intermittently concedes the gap when announcing token Ottoman-Turkish course offerings. The problem's existence is attested even by parties that favor the standing arrangement.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because the severance is deadweight loss, not transfer: the state collects no revenue from the barrier, and the only gains — scarce interpretive authority and paid mediation for the shrinking class that retains fluency — are marginal, erode with each cohort death and with improving machine transcription, and are held disproportionately by people campaigning to erase their own scarcity. Suppression is low (0.15) because the standing arrangement no longer forbids anything: the 1928-1950s bans on Arabic-script printing and the pressure on holdouts decayed into complete toleration; what remains is absence of provision, not presence of coercion. Theater_ratio (0.52) is the story's sharpest number: since the 1990s the arrangement's custodians and its challengers alike perform continuity — ceremonial calligraphy, neo-Ottoman aesthetics, token Ottoman-Turkish electives with thin uptake, anniversary rhetoric — while actual transmission investment (a teacher corps, mass coursework, transcription at scale) stays near zero; more than half of observable continuity activity is gesture. Accessibility_collapse (0.40) is deliberately below mountain range: alternatives do not vanish once the wall is understood — self-study takes months rather than years, university programs exist, transcriptions cover the canonical core, and OCR/LLM transcription is expanding — each route is real, merely costly. Resistance (0.55) records the active remediation front: digitization projects, civil-society courses, academic advocacy, recurring parliamentary proposals, and market forces eroding the barrier without any coordinator. The three measurement series share one six-point grid (1928-2026) so every metric is authored at every examined time point; base_extractiveness shows a mid-century scarcity hump then gentle decline; theater_ratio rises monotonically as enforcement gave way to performance; suppression_requirement falls monotonically (0.75 to 0.15) — this series is authored because the story specifically tracks enforcement-capacity decay, the bans-to-toleration arc being the arrangement's central enforcement history. Receipt check performed seat by seat before authoring gain_flow: the state collects nothing from the barrier; the constituency draws identity-resource from the grievance but demands the barrier's removal; residual experts accrue marginal, eroding mediation rents that do not shape the structure — no seat captures, hence the affirmative diffuse. Fixing_cost is prohibitive because the fix requires rebuilding a native-fluency teaching corps that is nearly extinct and adding a second script to universal schooling, for a benefit that is diffuse, politically contested, and discounted by the deciding seat, which itself bears almost none of the access cost. Boltzmann: identity_coordination names the arrangement's genuine surviving function — coordinating a bounded transmission community (master-apprentice calligraphic chains, clerical curricula, scholarly lineages) inside which the corpus stays readable; the type's complexity offset must not be allowed to launder the severance's cost at national scale, which is why the identity framing is flagged here as the reading's own rhetorical shell around a real but tiny coordination core.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical facts. From the agenda-setter seat the arrangement is settled modernization with a manageable heritage portfolio: the access gap is a rounding error against universal literacy, and remediation is a discretionary cultural expense. From the payer seats the same arrangement is a generational expropriation of inheritance, executed without consultation and never remedied. From the residual-beneficiary seat it is a shrinking professional monopoly the holders would gladly surrender. From the excluded historical seat it was an unanswerable decree. The engine derives these divergences from the power, horizon, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: post_reform_generations (listed victim, constrained exit, powerless) sit near the full-target end; residual_tradition_bearers (listed beneficiary, identity_locked) sit near the beneficiary end, damped further by their anti-barrier advocacy; the religious_conservative_constituency is dual-positioned — primary payer with a secondary identity-resource benefit — so an override sets the organized atom to d=0.60 rather than letting a single-listing derivation overshoot or undershoot. The turkish_republican_state appears in neither list; left to fallback its agenda-setter position would be scored neutrally or punitively, but its actual structural relationship is mild-beneficiary-by-construction — it authored the arrangement, bears almost none of the access cost, and can appropriate the heritage aesthetically at will — so an override sets the institutional atom to d=0.35. Scope is national: verifying the access gap is easy (the wall is visible to anyone who opens a pre-1928 ledger), so no large-scope amplification is claimed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the inherited corpus readable by the community — is not outlived; it is unfulfilled and worsening, so no mandatrophy is declared and the R5 status is live. The classification work here is different: it prevents two symmetrical mislabels. Read as grievance politics, the continuity claim invites a snare verdict ('we were robbed, someone profits') — but the receipt surface is affirmatively diffuse and the only rents are marginal and eroding, so the extraction story fails and the loss is what remains. Read as antiquarian nostalgia, the claim invites dismissal as a mountain-like fact of nature ('scripts just do not interread') — but the barrier was activated by a dated legislative choice, is forward-dissolvable at finite cost, and meets organized remediation resistance, so the natural-law story fails too. Piton holds both findings at once: real diffuse cost, no collector, an administrator who could act but bears too little to bother, and a maintenance surface that has become substantially theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story is one reading (continuity) of the orthographic_legitimacy_kernel; what would the sibling readings (instrumentalist, modernist) change structurally, and where exactly is the disagreement located?',
    'Author the sibling stories against the same standing arrangement and compare victim sets, epsilon, and computed types; the disagreement is located in the legitimacy criterion itself — what good an orthographic order serves (preserved access to the inherited corpus vs. literacy rates and administrative efficiency vs. alignment with Western modernity).',
    'Under the instrumentalist reading the same arrangement computes as a successful low-cost coordination standard with the severed corpus as acceptable collateral; under the modernist reading as a deliberate achievement; only under the continuity reading does the severance register as the central harm. Same facts, different constraints — cross-reading epsilon comparison is meaningless without this index.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this constraint is one reading of a three-reading kernel; sibling readings instantiate different constraints over the same arrangement.').

omega_variable(
    natural_barrier_vs_constructed_activation,
    'Is the access barrier a natural fact (distinct scripts do not interread) or a constructed constraint (the 1928 legislative choice activated it for this corpus-population pair)?',
    'Comparative counterfactual: polities that retained script continuity (Iran) show no equivalent generational cut-off; the non-interreadability of scripts is natural, but the salience of the barrier here traces to a dated statute.',
    'If constructed activation dominates, the barrier is not a natural limit, responsibility attaches to the choosing agent, and any mountain reading fails while the piton cost-asymmetry strengthens; if treated as natural infrastructure, the loss reads as misfortune rather than decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_barrier_vs_constructed_activation, empirical, 'Whether the severance barrier is natural law or choice-activated construction.').

omega_variable(
    remediation_technology_trajectory,
    'Will machine transcription and digitization dissolve the access barrier at scale within a generation, making the fix effectively cheap, or will expert-mediated custody persist?',
    'Track throughput and error rates of Ottoman-script OCR and LLM transcription against total untranscribed holdings, and uptake of self-service tools by non-specialist users.',
    'If technology dissolves the barrier, fixing_cost flips toward cheap, the piton reading becomes transient neglect, and the constraint decays toward moot; if mediation persists, the piton stabilizes and the scarcity-rent question sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_technology_trajectory, empirical, 'Whether transcription technology is rendering the barrier''s fix cheap.').

omega_variable(
    scarcity_rent_materiality,
    'Do the interpretive-authority rents of residual tradition-bearers constitute meaningful capture, or are they negligible and eroding?',
    'Income and caseload studies of professional Ottomanist mediation (transcription fees, expert testimony, manuscript authentication) measured against the volume of unmediated holdings.',
    'If material, gain_flow should name residual_tradition_bearers rather than diffuse, effective extraction rises, and the classification drifts toward tangled_rope; if negligible, the diffuse receipt stands and the piton reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_rent_materiality, empirical, 'Materiality of residual expert rents to the receipt surface.').

omega_variable(
    internalized_deference_component,
    'Is the remaining inaccessibility purely a competence cost, or does an internalized deference component (the belief that the old script is ''for specialists'') suppress demand for access even where cheap routes exist?',
    'Post-provision trajectory: measure uptake of free courses and free tools among interested laypeople; if uptake stays low where monetary and time cost approaches zero, the deference is internalized rather than structural.',
    'If internalized, effective suppression exceeds the structural measure and dissolution requires cultural change, not just provision; if purely structural, provision alone closes the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_deference_component, empirical, 'Structural versus internalized component of the residual access barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olkr_continuity_tr_t1928, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement_basis(olkr_continuity_tr_t1928, observed).
narrative_ontology:measurement(olkr_continuity_tr_t1948, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(olkr_continuity_tr_t1948, observed).
narrative_ontology:measurement(olkr_continuity_tr_t1968, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement_basis(olkr_continuity_tr_t1968, observed).
narrative_ontology:measurement(olkr_continuity_tr_t1988, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1988, 0.38).
narrative_ontology:measurement_basis(olkr_continuity_tr_t1988, observed).
narrative_ontology:measurement(olkr_continuity_tr_t2008, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2008, 0.48).
narrative_ontology:measurement_basis(olkr_continuity_tr_t2008, observed).
narrative_ontology:measurement(olkr_continuity_tr_t2026, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(olkr_continuity_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(olkr_continuity_be_t1928, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1928, 0.26).
narrative_ontology:measurement_basis(olkr_continuity_be_t1928, observed).
narrative_ontology:measurement(olkr_continuity_be_t1948, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1948, 0.28).
narrative_ontology:measurement_basis(olkr_continuity_be_t1948, observed).
narrative_ontology:measurement(olkr_continuity_be_t1968, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1968, 0.25).
narrative_ontology:measurement_basis(olkr_continuity_be_t1968, observed).
narrative_ontology:measurement(olkr_continuity_be_t1988, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1988, 0.23).
narrative_ontology:measurement_basis(olkr_continuity_be_t1988, observed).
narrative_ontology:measurement(olkr_continuity_be_t2008, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2008, 0.21).
narrative_ontology:measurement_basis(olkr_continuity_be_t2008, observed).
narrative_ontology:measurement(olkr_continuity_be_t2026, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2026, 0.2).
narrative_ontology:measurement_basis(olkr_continuity_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(olkr_continuity_su_t1928, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement_basis(olkr_continuity_su_t1928, observed).
narrative_ontology:measurement(olkr_continuity_su_t1948, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement_basis(olkr_continuity_su_t1948, observed).
narrative_ontology:measurement(olkr_continuity_su_t1968, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement_basis(olkr_continuity_su_t1968, observed).
narrative_ontology:measurement(olkr_continuity_su_t1988, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1988, 0.28).
narrative_ontology:measurement_basis(olkr_continuity_su_t1988, observed).
narrative_ontology:measurement(olkr_continuity_su_t2008, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2008, 0.2).
narrative_ontology:measurement_basis(olkr_continuity_su_t2008, observed).
narrative_ontology:measurement(olkr_continuity_su_t2026, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2026, 0.15).
narrative_ontology:measurement_basis(olkr_continuity_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the alphabet reform debate' conflates three structurally distinct legitimacy claims; per the epsilon-invariance principle they are authored as separate stories forming the orthographic_legitimacy_kernel family, linked through affects_constraints. The instrumentalist reading is upstream: its efficiency case was the operative justification that produced the 1928 statute, and it thereby created the severance this downstream reading mourns; the modernist reading supplied the era's value-frame. Epsilon differs across the family because each reading assesses the SAME standing arrangement against a different referent-good — the values are reading-indexed over a fixed referent, not measurements of different objects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, institutional, 0.35).
constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
