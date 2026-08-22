% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Reading: 'Amal Ahl al-Madina as Living-Tradition Legal Source
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   Malik ibn Anas and the Medinan legal tradition treated the consensus
 *   practice of Medina's scholars and community as direct evidence of the
 *   Prophet's actual sunna, sometimes weighted above individual hadith
 *   reports that conflicted with it, on the reasoning that a whole
 *   community's continuous practice is harder to fabricate or corrupt than a
 *   single transmission chain. This gave Maliki jurisprudence a distinctive
 *   source of law unavailable to schools without a comparable claim to
 *   geographic-communal continuity, and it structurally privileged Medinan
 *   scholars and their students over jurists trained in Kufa, Basra,
 *   Damascus, or later in al-Andalus who could offer only textual
 *   transmission, not communal practice, as their warrant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Reading: 'Amal Ahl al-Madina as Living-Tradition Legal Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'a467d54d-18cb-41b5-b0e1-eba9707b0a53').
narrative_ontology:cs_kernel_codification('a467d54d-18cb-41b5-b0e1-eba9707b0a53', fixed_text).
narrative_ontology:cs_authority_grounding('a467d54d-18cb-41b5-b0e1-eba9707b0a53', lineage).
narrative_ontology:cs_interpretation_layer_present('a467d54d-18cb-41b5-b0e1-eba9707b0a53').
narrative_ontology:cs_reading_relation('a467d54d-18cb-41b5-b0e1-eba9707b0a53', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a467d54d-18cb-41b5-b0e1-eba9707b0a53', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('a467d54d-18cb-41b5-b0e1-eba9707b0a53', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('a467d54d-18cb-41b5-b0e1-eba9707b0a53', foundational, medinan_communal_continuity_constitutes_sunna_evidence).
narrative_ontology:cs_axiom_status(medinan_communal_continuity_constitutes_sunna_evidence, holdable).
narrative_ontology:cs_axiom_grounding('a467d54d-18cb-41b5-b0e1-eba9707b0a53', medinan_communal_continuity_constitutes_sunna_evidence, conventional).
narrative_ontology:cs_axiom('a467d54d-18cb-41b5-b0e1-eba9707b0a53', secondary, amal_overrides_conflicting_ahad_hadith).
narrative_ontology:cs_axiom_status(amal_overrides_conflicting_ahad_hadith, holdable).
narrative_ontology:cs_axiom_grounding('a467d54d-18cb-41b5-b0e1-eba9707b0a53', amal_overrides_conflicting_ahad_hadith, instrumental).
narrative_ontology:cs_reference_frame('a467d54d-18cb-41b5-b0e1-eba9707b0a53', medinan_communal_practice_as_direct_sunna_evidence).
narrative_ontology:cs_drift_state('a467d54d-18cb-41b5-b0e1-eba9707b0a53', abbasid_era_school_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a467d54d-18cb-41b5-b0e1-eba9707b0a53', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurisconsults).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, iraqi_and_syrian_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, ordinary_litigants).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, ordinary_litigants).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, medina_preserved_prophetic_practice_most_faithfully).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and administers 'amal ahl al-Madina as the living record of the Prophet's community continuing his practice generation after generation. Their consensus on practice functions as a source of law independent of, and sometimes correcting, individual hadith transmissions. Their geographic and genealogical position in Medina is the entire warrant for their authority; they cannot be displaced by a jurist with a stronger isnad elsewhere because the claim is about communal continuity, not chain-of-narrators strength.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Build careers, courts, and fatwas on Malik's Muwatta and the methodological priority of Medinan practice over solitary hadith reports (ahad) that conflict with it. Their professional standing and the legal certainty they can offer clients depend on 'amal remaining a recognized, non-negotiable source; abandoning it would collapse a large share of settled Maliki rulings back into contested status.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurisconsults, beneficiary,
    organized, generational, constrained, continental).

% Jurists and communities outside Medina who preserved authentic prophetic hadith through rigorous isnad-criticism find their transmitted reports subordinated whenever they conflict with what Medina is said to have practiced. They cannot relocate their scholarly lineage to Medina after the fact, and no amount of isnad rigor lets them out-rank 'amal on its own terms — the geographic gate is closed to them permanently.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    organized, generational, constrained, continental).

% Kufan and Syrian jurisprudential traditions built substantial reasoning apparatuses (proto-Hanafi qiyas, Syrian ahl al-hadith practice) that get treated as secondary or corrective-target whenever they diverge from asserted Medinan norms. Their exit is theoretical only — they can develop rival schools (which happened, historically) but cannot contest the Maliki claim to superior authenticity from within the Maliki framework itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, iraqi_and_syrian_jurists, payer,
    organized, generational, constrained, continental).

% Receive rulings grounded in a claimed continuous, communally-verified practice rather than an isolated report that might be forged or misremembered — a genuine coordination benefit in legal predictability. But where 'amal encodes a locally contingent custom no longer functional or fair outside the Hijaz, litigants in Maliki jurisdictions bear rulings shaped by a specific 8th-century Medinan social context with no mechanism to contest that the practice's authenticity claim itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, ordinary_litigants, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, ordinary_litigants, payer).

% Study the historical record of early Islamic legal development, including source-critical scholarship (e.g. Schacht) questioning whether 'amal ahl al-Madina reflects continuous Prophetic practice or later Medinan juristic consensus retrojected onto the Prophet. They can examine documentary and comparative evidence the tradition itself does not treat as admissible.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving legal questions where hadith transmission is ambiguous, contradictory, or thin, by appeal to the continuous lived practice of the community closest in space and time to the Prophet — solving a genuine epistemic problem (which reports are reliable) with a genuine epistemic resource (communal continuity of practice).
% TRANSFER_FUNCTION: Moves interpretive authority and the legitimacy premium that comes with being recognized as an authentic source of law toward Medinan scholars and those trained in their tradition, and away from jurists elsewhere whose hadith-based or reason-based claims to authenticity are treated as subordinate when they conflict with asserted Medinan norms.
% ABSENT_VOICES: Kufan, Syrian, and later Andalusian jurists whose independent transmission chains or reasoning traditions would contest that geographic proximity to Medina is a reliable proxy for practice-fidelity are not positioned to overturn the premise from within the Maliki framework — the framework's own axiom forecloses their objection before it can be heard on equal terms.
% DISAPPEARANCE_RATIONALE: If 'amal ahl al-Madina lost its status as an independent source, a substantial body of Maliki rulings that currently override conflicting ahad hadith would need re-derivation from hadith and qiyas alone, converging the Maliki school's method much closer to Shafi'i's hierarchy — courts, fatwas, and legal education built on Malik's Muwatta would need to renegotiate their own authority.
% FOUNDING_PROBLEM: In the first Islamic century, hadith transmission was uneven, sometimes contradictory, and vulnerable to fabrication; jurists needed a reliable way to distinguish authentic prophetic practice from spurious or garbled reports, especially where reports conflicted with each other.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists themselves attest the problem remains live: reports still conflict and 'amal still functions as the tie-breaker. Outside the tradition, source-critical historians (following lines of inquiry associated with Joseph Schacht and later revisionist and traditionalist historiography alike) attest that the 'continuous practice' claim is itself partly a retrospective juristic construction rather than a transparent historical record, meaning the very tool proposed to solve the authenticity problem carries its own unresolved authenticity problem.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon=0.48) sits at medium: the coordination function is real — resolving conflicting or thin hadith evidence via communal practice is a genuine method for approaching authenticity, not a naked power grab — but the authority premium captured by Medinan-lineage scholars, and the corresponding discount applied to equally rigorous non-Medinan transmission chains, is a real asymmetric transfer riding on top of that coordination function. Suppression (0.42) reflects that non-Medinan jurists are not silenced by force, but their strongest evidentiary tool (isnad rigor) is structurally subordinated whenever it conflicts with asserted 'amal, which is a real, if soft, foreclosure. Accessibility collapse is moderate (0.5): a jurist can still argue against a specific claimed 'amal on evidentiary grounds, so the alternative is not fully closed off, but the geographic gate itself (only Medina counts as maximally authoritative) cannot be argued away from inside the framework. Theater ratio rises over the interval (0.10 to 0.28) as later Maliki jurisprudence increasingly invokes 'amal as settled doctrinal shorthand rather than as a live, checkable claim about actual observed practice — a mild Goodhart drift from evidentiary tool to institutional trump card.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan scholarly lineage and Maliki jurisconsults sit near the beneficiary end: their authority claim is validated by the very mechanism under study, and their institutional position (schools, courts, teaching chains) both produces and is produced by the doctrine. Non-Medinan interpretive claims and Iraqi/Syrian jurists sit near the target end: their competing evidentiary claims are structurally discounted by a criterion (geographic-communal continuity) they cannot retroactively satisfy, regardless of their transmission rigor — this is a durable, non-negotiable structural disadvantage, which is why their exit_options are 'constrained' rather than 'trapped' (they can found or join rival schools, which is what historically happened) but never 'mobile' within the Maliki framework itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing authentic from corrupted hadith transmission in an oral-transmission environment — was genuinely live in the first Islamic century, which is why this is authored as tangled_rope rather than snare: there is a real coordination function, not merely an extraction story dressed as one. But the founding_problem_status is contested rather than resolved-live: later Maliki jurisprudence continues to invoke 'amal as though the fabrication-detection problem it was built to solve remains as urgent as it was for a first-generation community, even where subsequent isnad-criticism methodology (developed largely by hadith scholars outside the Maliki mainstream) offers alternative, more portable tools for the same authenticity problem. The disappearance_verdict of world_rearranges, combined with contested founding-problem status, is exactly the pattern the framework is built to flag: a real original coordination function, partially persisting past the point where better tools existed to solve the same problem, propping up an asymmetric authority structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amal_historical_authenticity,
    'Does ''amal ahl al-Madina, as invoked by later Maliki jurists, actually reflect continuous, verifiable Prophetic-era practice, or is it substantially a retrospective juristic construction attributing later Medinan consensus to earlier authority?',
    'Comparative source-critical analysis of early Medinan legal texts against independently attested hadith and non-Medinan legal traditions from the same period; assessment of whether claimed continuity survives scrutiny comparable to isnad-criticism applied to hadith.',
    'If ''amal substantially reflects genuine continuous practice, the coordination function is stronger than the extraction reading suggests and epsilon should be lower. If it is substantially retrospective construction, the beneficiary asymmetry is closer to a snare dressed in coordination language and epsilon should be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amal_historical_authenticity, empirical, 'Whether the Medinan living-tradition claim is historically verifiable continuity or retrospective doctrinal construction.').

omega_variable(
    geographic_authenticity_proxy_validity,
    'Is geographic-communal proximity to Medina a valid proxy for practice-fidelity to the Prophet''s sunna, or is it a contingent, non-transferable criterion that happens to privilege whoever is already positioned there?',
    'Comparative jurisprudential analysis of whether other schools'' independent authenticity criteria (isnad rigor, multiple independent attestation) achieve comparable or superior reliability without the geographic dependency; historical study of population movement and scholarly migration patterns that would test whether ''Medinan practice'' remained stable or was itself subject to drift and contest within Medina.',
    'If the proxy is structurally arbitrary (a location-based criterion with no principled epistemic superiority over rigorous transmission-chain criticism), the Maliki reading''s exclusion of equally rigorous non-Medinan claims looks more like an entrenched first-mover advantage than a genuine epistemic filter — raising the extraction reading. If the proxy has genuine epistemic force, the coordination story is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_authenticity_proxy_validity, conceptual, 'Whether geographic proximity to Medina is a principled or arbitrary criterion for authenticity.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the jurisprudential_method_kernel most usefully framed at the level of ''which sources count as valid law'' (the framing used here and across all four sibling readings) or at a finer grain distinguishing ''amal as evidentiary tool from ''amal as trump-card doctrine — which would itself split the Maliki reading into an early (evidentiary, lower-extraction) and later (doctrinal, higher-extraction) sub-reading?',
    'Track whether Maliki legal literature itself marks a transition point where ''amal shifts from an argued, checkable claim to a citation of settled precedent; if a clear transition is documented, author it as two linked constraint stories rather than one story with a rising theater_ratio.',
    'If the finer-grained split is warranted, this single story''s epsilon (0.48, averaged across the interval) would decompose into a lower-epsilon early-Maliki reading and a higher-epsilon later-Maliki reading, per the ε-invariance principle. The current single-story treatment with a rising theater_ratio measurement series is a compromise pending that decomposition decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether the diachronic drift in how ''amal is invoked warrants decomposing this single reading into two linked constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 700, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__maliki_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement_basis(juri_tr_t700, projected).
narrative_ontology:measurement(juri_tr_t740, jurisprudential_method_kernel__maliki_reading, theater_ratio, 740, 0.14).
narrative_ontology:measurement_basis(juri_tr_t740, projected).
narrative_ontology:measurement(juri_tr_t780, jurisprudential_method_kernel__maliki_reading, theater_ratio, 780, 0.18).
narrative_ontology:measurement_basis(juri_tr_t780, projected).
narrative_ontology:measurement(juri_tr_t820, jurisprudential_method_kernel__maliki_reading, theater_ratio, 820, 0.22).
narrative_ontology:measurement_basis(juri_tr_t820, projected).
narrative_ontology:measurement(juri_tr_t860, jurisprudential_method_kernel__maliki_reading, theater_ratio, 860, 0.25).
narrative_ontology:measurement_basis(juri_tr_t860, projected).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.28).
narrative_ontology:measurement_basis(juri_tr_t900, projected).

% Extraction over time
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 700, 0.3).
narrative_ontology:measurement_basis(juri_be_t700, projected).
narrative_ontology:measurement(juri_be_t740, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 740, 0.36).
narrative_ontology:measurement_basis(juri_be_t740, projected).
narrative_ontology:measurement(juri_be_t780, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 780, 0.41).
narrative_ontology:measurement_basis(juri_be_t780, projected).
narrative_ontology:measurement(juri_be_t820, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 820, 0.44).
narrative_ontology:measurement_basis(juri_be_t820, projected).
narrative_ontology:measurement(juri_be_t860, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 860, 0.46).
narrative_ontology:measurement_basis(juri_be_t860, projected).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.48).
narrative_ontology:measurement_basis(juri_be_t900, projected).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 700, 0.2).
narrative_ontology:measurement_basis(juri_su_t700, projected).
narrative_ontology:measurement(juri_su_t740, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 740, 0.26).
narrative_ontology:measurement_basis(juri_su_t740, projected).
narrative_ontology:measurement(juri_su_t780, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 780, 0.32).
narrative_ontology:measurement_basis(juri_su_t780, projected).
narrative_ontology:measurement(juri_su_t820, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 820, 0.36).
narrative_ontology:measurement_basis(juri_su_t820, projected).
narrative_ontology:measurement(juri_su_t860, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 860, 0.39).
narrative_ontology:measurement_basis(juri_su_t860, projected).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.42).
narrative_ontology:measurement_basis(juri_su_t900, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of jurisprudential_method_kernel, each authored as its own constraint with its own epsilon and beneficiary/victim structure: hanafi_reading (reasoned extension via qiyas/istihsan), maliki_reading (this file — Medinan living-practice priority), shafii_reading (strict four-tier hierarchy privileging hadith-transmission rigor), and hanbali_reading (literal text plus unanimous consensus, treating reasoned innovation as corruption). The four readings are linked via affects_constraints rather than merged, per the ε-invariance principle: each reading's beneficiary/victim structure and extraction profile differ (Medinan lineage vs. Kufan/reasoning-based jurists vs. hadith-transmission specialists vs. literalist traditionists), so no single epsilon value could honestly represent all four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
