% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Legitimacy - Modernist Reading (Western Alignment and Civilizational Rupture)
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   A post-imperial state redefines orthographic legitimacy itself: a piece
 *   of writing counts as legitimate, official, and modern only insofar as it
 *   aligns with Western/European norms and breaks with the Ottoman/Islamic
 *   past. Overnight, the Arabic-letter script is removed from official life;
 *   the trained literate class is rendered functionally illiterate in its own
 *   country; the entire pre-reform corpus is sealed behind state-controlled
 *   transliteration; and a new intelligentsia is credentialed in the
 *   replacement script. This file instantiates ONLY the modernist reading of
 *   the orthographic-legitimacy kernel (Rule 1): one clean, epsilon-invariant
 *   constraint. The eps referent is the standing arrangement under contest -
 *   the Latin-legitimacy regime itself - assessed by this reading's own
 *   lights: the modernist frame affirms the rupture's purpose yet authors
 *   high extraction because the arrangement demonstrably confiscates one
 *   class's accumulated capital to endow another; affirming the project is
 *   not the same as denying what it extracts. The colloquial label
 *   'orthographic legitimacy' decomposes into three structurally distinct
 *   claims (this reading, a continuity reading, an instrumentalist reading),
 *   each authored as a separate file and linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: agenda-setter and principal collector (institutional/arbitrage) - writes, enforces, and profits from the orthographic order
 *   - republican_new_intelligentsia: primary beneficiary (organized/mobile) - credentialed into positions the purge vacated
 *   - ottoman_literate_class: primary target (moderate/identity_locked) - rendered functionally illiterate, corpus access severed
 *   - islamic_religious_scholars: primary target (moderate/identity_locked) - textual authority detached from state infrastructure
 *   - new_script_literate_public: secondary beneficiary with diffuse costs (moderate/constrained) - cheap literacy bought at the price of mediated memory
 *   - ottoman_exile_writers: excluded voice (moderate/mobile) - maintains the continuity the official narrative denies
 *   - script_reform_historians: analytical observer (analytical/analytical) - sees the full structure from outside the credential system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.74).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.52).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Legitimacy - Modernist Reading (Western Alignment and Civilizational Rupture)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political linguistics / state formation / commitment systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, 'efe55a07-e731-4f9b-b831-5eca624bf1e7').
narrative_ontology:cs_kernel_codification('efe55a07-e731-4f9b-b831-5eca624bf1e7', formalized).
narrative_ontology:cs_authority_grounding('efe55a07-e731-4f9b-b831-5eca624bf1e7', extraction).
narrative_ontology:cs_interpretation_layer_present('efe55a07-e731-4f9b-b831-5eca624bf1e7').
narrative_ontology:cs_reading_relation('efe55a07-e731-4f9b-b831-5eca624bf1e7', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('efe55a07-e731-4f9b-b831-5eca624bf1e7', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('efe55a07-e731-4f9b-b831-5eca624bf1e7', foundational, civilizational_rupture_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(civilizational_rupture_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('efe55a07-e731-4f9b-b831-5eca624bf1e7', civilizational_rupture_constitutes_legitimacy, deontological).
narrative_ontology:cs_axiom('efe55a07-e731-4f9b-b831-5eca624bf1e7', foundational, archive_severance_precondition_of_rebirth).
narrative_ontology:cs_axiom_status(archive_severance_precondition_of_rebirth, holdable).
narrative_ontology:cs_axiom_grounding('efe55a07-e731-4f9b-b831-5eca624bf1e7', archive_severance_precondition_of_rebirth, instrumental).
narrative_ontology:cs_reference_frame('efe55a07-e731-4f9b-b831-5eca624bf1e7', civilizational_rupture_baseline).
narrative_ontology:cs_drift_state('efe55a07-e731-4f9b-b831-5eca624bf1e7', contemporary_neo_ottoman_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('efe55a07-e731-4f9b-b831-5eca624bf1e7', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, republican_new_intelligentsia).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_public).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, islamic_religious_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_public).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, civilizational_rupture_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, west_as_measure_of_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the ministries, schools, courts, and press regulation through which the new alphabet is imposed. Writes the curriculum, licenses publications, staffs the transliteration bureaus that decide which parts of the pre-reform corpus become readable, and disciplines officials who draft in the old script. Collects the legitimacy dividend: every credential, archive key, and public office it issues passes through the order it founded. It wrote the rule and retains the capacity to rewrite it, though rewriting would undercut its own founding act.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, beneficiary).

% Teachers, journalists, lawyers, and academics formed inside the new script. Their diplomas, careers, and social standing are denominated in the reformed orthography; the displacement of the old literate class cleared the positions they now hold. They defend the order in print and parliament without administering its enforcement machinery. Leaving would mean retraining abroad or accepting reduced status; few ever need to.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, republican_new_intelligentsia, beneficiary,
    organized, biographical, mobile, national).

% Poets, historians, clerks, and calligraphers trained over years in the Arabic-letter Ottoman script. After the switch their skill set is legally and socially worthless: they cannot read new newspapers, fill new forms, or certify new documents, and the corpus they mastered is sealed unless a state bureau translates it. Their identity and livelihood were built on the severed tradition; learning the new letters late in life restores mechanical literacy, not standing. Some retrain as proofreaders of official transliterations; most withdraw from public textual life.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    moderate, generational, identity_locked, national).

% Ulema and mosque teachers whose interpretive authority rests on Arabic-script transmission of scripture, jurisprudence, and Ottoman religious administration. The reform severs them from the state's textual infrastructure and marks their literacy as backward; preaching licenses and endowed foundations fall under ministries staffed by graduates of the new script. Their reference community crosses borders, but inside the country every official channel runs through an orthography they were ordered to abandon.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, islamic_religious_scholars, payer,
    moderate, generational, identity_locked, continental).

% Citizens schooled after the reform. The Latin letters are easy to learn and they reach working literacy faster than their parents' generation did. They also inherit a national library they cannot open without translation, and their picture of the past arrives filtered through state textbooks. They neither administer the arrangement nor conspicuously profit from it; it is the environment they were formed in.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_public, payer).

% Authors and journalists who left after the reform and continue publishing in Ottoman script from Cairo, Paris, and other emigration centers. Inside the country their books circulate marginally; abroad they keep the severed literature alive for a shrinking readership. They are the conversation the reform structurally excludes: their continuing output demonstrates the continuity the official narrative declares impossible.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_exile_writers, excluded,
    moderate, generational, mobile, continental).

% Linguists and historians who study script reforms comparatively. They read both scripts, sit outside the national credential system, and document what was gained, lost, and concealed. Their analyses circulate in academic venues the domestic disputants rarely consult.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, script_reform_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single standardized, phonetic orthography that coordinates print, schooling, and administration across the national community, and lowers the acquisition cost of literacy for new learners relative to the old digraphic inheritance.
% TRANSFER_FUNCTION: Moves textual authority, cultural capital, and the credential that counts as 'literate' and 'modern' from holders of Ottoman/Arabic literacy to the state apparatus and its newly credentialed intelligentsia; it also moves the nation's usable written past into state custody behind transliteration bureaus.
% ABSENT_VOICES: Ottoman exile writers, Arabic-script printers barred from the licensed press, and the future generations who will meet their own archives only through state-mediated translation would all object; they stand outside the republican public sphere the reform created - in emigration centers, unlicensed backrooms, and the sealed archive stacks.
% DISAPPEARANCE_RATIONALE: If the modernist orthographic order vanished overnight, administration, schooling, print, and credentialing would all reorganize: the script question would reopen politically, existing credentials would be stranded, archive-access politics would invert, and the state would lose the legitimacy instrument it built its identity narrative on.
% FOUNDING_PROBLEM: Consolidating a post-imperial national identity distinct from the Ottoman/Islamic order and signaling an irreversible Western course; the script was chosen because typographic rupture is fast, visible, verifiable daily by every citizen, and hard to reverse.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: contemporary foreign diplomatic correspondence recorded the reform's identity-political aims alongside its stated pedagogic ones; subsequent archival historiography by scholars with no stake in the regime's legitimacy narrative documents the deliberate targeting of the ulema's textual base; and the published memoirs of displaced Ottoman men of letters attest the dispossession from the losing side. The benefiting parties' own account (pure efficiency and progress) is partially contradicted by each of these sources.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.74 at interval end) because the arrangement's core act was a class-wide confiscation of cultural capital - an entire literate stratum's skills devalued to zero by legal fiat - followed by a permanent structural toll: every subsequent generation pays for access to its own past through state translation. Suppression (0.52) is authored as a RAW structural property, unscaled; the engine scales only extraction. The suppression REQUIREMENT series traces the enforcement arc the story actually exhibits: a sharp build-up in the first fifteen years (compulsory night schools, fines, inspections, press controls), then a partial release as the norm became self-enforcing through schooling and habitus, settling into professionalized gatekeeping (publication licensing, curriculum control, contested Ottoman-elective introductions decades later) rather than full withdrawal. Theater rises monotonically (0.20 -> 0.43) as the transformation's real work completed and commemoration, anniversary rhetoric, and civilizational self-description grew as a share of activity around the arrangement. Accessibility collapse (0.62) reflects foreclosure of dual-literacy, gradual-transition, and private old-script alternatives - short of totality, since manuscripts survive and diaspora circles persist. Resistance (0.55) was real (press debates, religious opposition, passive non-conversion by the older cohort) and ultimately marginalized. The three series share one time grid (t = 0,15,30,45,60,75,90; unit approximates years since enactment, t0 = the 1928-pattern founding act) as required; no cyclical dynamics are claimed - all three trajectories are monotonic arcs.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the agenda-setter seat the structure is a founding achievement it administers and profits from - coordination it owns. From the two payer seats the identical structure operated as confiscation and continues to operate as exile from their own tradition, amplified by identity-lock: their exit would require dissolving the selves the constraint is made of. The new intelligentsia seat experiences meritocratic opening. Same-nominal-level dynamics matter: ottoman_literate_class and islamic_religious_scholars hold the same power atom and the same exit atom, but their locks differ in kind - the scholars' fusion is to a transnational Arabic textual chain (exit unthinkable without abandoning their interpretive office), the literati's to a national high culture (partial conversion to translator-status was survivable for a few). The engine computes these divergences from the authored structural data; the claimed type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is declared in both beneficiaries and as agenda_setter with arbitrage exit: derived d sits nearest the beneficiary pole and its effective extraction is damped despite its power. The republican intelligentsia, a pure beneficiary with mobile exit, likewise derives low d. The two victim classes derive high d, pushed further toward the full-target pole by identity_locked exit. The literate public carries a mixed declaration (beneficiary with payer secondary role): genuine cheap literacy against diffuse heritage-mediation costs, deriving a near-symmetric d. NO directionality overrides are authored: the override mechanism keys on power_atom alone, and this story has three seats at 'moderate' (two full targets, one near-symmetric, one excluded) whose directionalities differ - a moderate-level override would smear them together. Their differentiation is carried instead by role and exit_options, which the derivation chain reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - consolidating post-imperial identity and signaling an irreversible Western course - was plausibly accomplished within two generations; the arrangement nonetheless persists with rising theater and a settled enforcement plateau. founding_problem_status is authored 'contested' rather than 'dead' because the parties genuinely dispute obsolescence: the modernist coalition reads the mandate as ongoing (integration pressures), while continuity-oriented actors read it as long finished and the arrangement as pure legacy cost. Because status is contested, the status x verdict mismatch consumer should not fire an automatic zombie flag here - but the theater trajectory (0.20 to 0.43) is the drift signature worth watching: if commemoration continues substituting for function, the structure slides toward piton dynamics; if enforcement re-intensifies around the old script's residual uses, toward snare dynamics. mandatrophy_resolved is deliberately left unset. The tangled_rope claim prevents both mislabelings: pure-snare coding would erase the real coordination delivered (standardized phonetic literacy, unified administration); pure-rope coding would erase the targeted dispossession that was constitutive, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the modernist reading of orthographic_legitimacy_kernel; which structural features authored here belong to the reading rather than the kernel, and how would classification move under the sibling readings over the same standing referent?',
    'Generate and compile the sibling stories (continuity_reading, instrumentalist_reading) and compare per-seat classifications and eps over the fixed referent; the deltas localize what is reading-indexed.',
    'Under the continuity reading the victim set expands (all post-reform generations severed from the corpus join the targets) and eps likely rises; under the instrumentalist reading eps falls toward coordination-cost levels and the structure may compute closer to rope. Classification of this file is conditional on the reading, not the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: reading-relative structure versus kernel-invariant structure.').

omega_variable(
    constitutive_vs_instrumental_cover,
    'Was script change genuinely constitutive of identity transformation as this reading claims, or was the civilizational-rupture rhetoric a cover story for administratively convenient simplification (the instrumentalist sibling''s account)?',
    'Archival study of the language council and cabinet deliberations: sequencing and weight of efficiency arguments versus identity arguments in the reformers'' private reasoning, and whether a less disruptive orthography was rejected despite meeting efficiency goals.',
    'If rupture rhetoric was cover, the target-selection logic shifts from class identity to transition-cost incidence, this reading collapses toward the instrumentalist sibling, and both authored axioms lose their grounding; if constitutive, the extraction pattern is irreducibly identity-directed and the tangled_rope asymmetry is confirmed at its deepest layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_instrumental_cover, empirical, 'Whether the identity-coordination function is genuine or an extraction cover story (FNL gaming check for identity_coordination).').

omega_variable(
    public_seat_net_position,
    'Did the new-script literate public benefit net (cheap literacy, standardization gains) or were they conscripted into a legitimacy machine whose heritage-severance costs exceed their individual gains?',
    'Longitudinal comparison of literacy returns and economic mobility for post-reform cohorts against valuations of lost cultural access (archive-use rates, translation dependence, measured demand for heritage literacy when offered).',
    'If net costs dominate, the public seat''s derived directionality moves from near-symmetric toward the target pole, enlarging the extracted class and strengthening the extraction reading; if net gains dominate, the coordination half of the tangled_rope is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_seat_net_position, empirical, 'Net position of the dual-role public seat (beneficiary with payer secondary role).').

omega_variable(
    persistence_source_after_enforcement_decay,
    'With the suppression series decaying from its early peak yet the arrangement fully stable, what now sustains it - habituated self-enforcement and institutional reproduction (inertial persistence) or continuing active rent capture by the state apparatus?',
    'Counterfactual probing of restoration and dual-literacy proposals: track whether archive-access liberalization and heritage-script initiatives fail because of popular indifference (inertia) or because of state veto and licensing refusal (capture).',
    'Inertia-dominated persistence signals piton-direction drift for the arrangement''s later lifecycle; capture-dominated persistence confirms the named receipt seat and keeps the structure snare-adjacent within its tangled_rope band; either result recalibrates fixing_cost interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_source_after_enforcement_decay, empirical, 'What sustains the constraint after active enforcement receded.').

omega_variable(
    cs_kernel_codification_ambiguity,
    'Is the adjudicating kernel of this commitment system the codified statute-and-curriculum complex (formalized, ministry-interpreted), or the distributed modernist narrative carried by the intelligentsia itself (implicit, no designated interpreter)?',
    'Trace where attempted revisions actually land: statutory or curricular amendment channels versus diffuse narrative shifts among intellectuals; observe which channel a challenge to orthographic legitimacy must traverse to succeed.',
    'Under the distributed framing, interpretation_layer_present becomes invalid, authority_grounding shifts toward distributed, and the drift vector would be located in the intelligentsia''s narrative practice rather than in ministry gatekeeping; under the codified framing the authored cs_structure stands as-is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_codification_ambiguity, conceptual, 'CS-framing under-determination: codified-statute kernel versus distributed-narrative kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(orth_tr_t45, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(orth_tr_t60, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(orth_tr_t75, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement(orth_tr_t90, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 90, 0.43).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.84).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(orth_be_t45, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 45, 0.78).
narrative_ontology:measurement(orth_be_t60, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(orth_be_t75, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 75, 0.75).
narrative_ontology:measurement(orth_be_t90, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 90, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(orth_su_t45, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 45, 0.64).
narrative_ontology:measurement(orth_su_t60, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(orth_su_t75, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement(orth_su_t90, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 90, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'orthographic legitimacy' decomposes per the eps-invariance principle into three structurally distinct claims sharing one standing referent (the Latin-script legitimacy regime) but carrying different authored eps, victim sets, and types. This file (modernist_reading) is downstream of the continuity_reading in discursive influence - modernizing elites cite the tradition's supposed burden as evidence for rupture - while it stands in a coexistence relation to the instrumentalist_reading, whose efficiency evidence the modernist coalition recruited as justification. Each member links to the others; orphaning any one would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
