% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Orientalist Codification of 'Hindu Law'
 *   domain: religious/hermeneutic/legal-colonial
 *
 * SUMMARY:
 *   Between Hastings's 1772 judicial plan and the Hindu Code Acts of 1955-56,
 *   the colonial state operated on the claim that the Vedic and Dharmashastra
 *   corpus constitutes a single, timeless 'Hindu law' that administrators
 *   could recover by philology and enforce by court. This story instantiates
 *   the colonial orientalist reading of the contested kernel
 *   vedic_corpus_social_prescription: the corpus-as-code claim that made the
 *   texts administrable. The ε referent is the standing arrangement under
 *   contest — the codification-and-enforcement apparatus itself — not any
 *   endorsed alternative. The claim/metric gap is deliberate: the reading
 *   presents itself as neutral transmission of discovered law, while the
 *   authored metrics describe an arrangement with a genuine coordination core
 *   (legibility for governance) wrapped around asymmetric extraction (frozen
 *   hierarchy, displaced custom, subordinated subjects). Family links run to
 *   the two sibling readings and to the census-classification apparatus this
 *   reading made possible. KEY AGENTS (by structural relationship): -
 *   colonial_administration: Agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — declares the canon, runs the courts, collects
 *   governability - anglo_indian_legal_establishment: Secondary beneficiary
 *   (powerful/constrained) — careers and authority built on the codified
 *   corpus - brahmin_pandit_intermediaries: Dual-positioned collaborator
 *   elite (moderate/identity_locked) — elevated, paid, then absorbed and
 *   abolished - upper_caste_landholding_elites: Incidental beneficiary
 *   (powerful/constrained) — precedence confirmed in writing -
 *   lower_caste_and_outcaste_communities: Primary target (powerless/trapped)
 *   — bears the codified hierarchy - hindu_women_subject_to_textual_law:
 *   Primary target (powerless/trapped) — status decided by selected textual
 *   defaults - customary_village_jurisdictions: Displaced coordination
 *   provider (organized/constrained) — loses jurisdiction to the courts -
 *   anti_caste_and_nationalist_critics: Excluded voice
 *   (organized/constrained) — objects only after the categories are fixed -
 *   hermeneutic_scholars_postcolonial: Analytical observer
 *   (analytical/analytical) — external reconstruction of the construction
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.5).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.4).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, tangled_rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Codification of 'Hindu Law'").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious/hermeneutic/legal-colonial").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'a7c9698f-aa67-4e88-9cfc-4fb277ef91ab').
narrative_ontology:cs_kernel_codification('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', fixed_text).
narrative_ontology:cs_authority_grounding('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', expertise).
narrative_ontology:cs_interpretation_layer_present('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab').
narrative_ontology:cs_reading_relation('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', foundational, scriptural_corpus_constitutes_binding_law).
narrative_ontology:cs_axiom_status(scriptural_corpus_constitutes_binding_law, overridden).
narrative_ontology:cs_axiom_grounding('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', scriptural_corpus_constitutes_binding_law, empirically_contingent).
narrative_ontology:cs_axiom('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', foundational, philological_access_supersedes_practice).
narrative_ontology:cs_axiom_status(philological_access_supersedes_practice, holdable).
narrative_ontology:cs_axiom_grounding('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', philological_access_supersedes_practice, instrumental).
narrative_ontology:cs_reference_frame('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', unified_timeless_scriptural_law_code).
narrative_ontology:cs_drift_state('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', post_independence_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7c9698f-aa67-4e88-9cfc-4fb277ef91ab', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_indian_legal_establishment).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_intermediaries).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_and_outcaste_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_subject_to_textual_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, customary_village_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_intermediaries).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_philological_method).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, trusteeship_non_interference_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the courts, revenue system, and later the census that operate the codified law. Declares which texts count as authoritative, commissions the translations, appoints and dismisses the pandit officers, and staffs the judiciary. Gains a governable population: uniform rules it can apply without ethnographic command of local practice, legible categories for taxation and policing, and a legitimating account in which it merely administers India's own ancient law. Its personnel rotate home on schedule; the institution itself faces no comparable confinement.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Judges, law members, translator-scholars, and the Fort William College professoriate whose careers, publications, and reputations are built on recovering and expounding the codified corpus. Their professional capital is specific to the arrangement: an Orientalist or judge who leaves India returns to a home market that values the expertise far less. They collect salaries, honors, and scholarly authority from the system's continuation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_indian_legal_establishment, beneficiary,
    powerful, biographical, constrained, continental).

% Serve as court officers supplying Sanskrit authority for judgments and as informants and collaborators for the translating scholars. The arrangement elevates their textual office above rival customary authorities and pays them for it; over time the printed digests displace them, their opinions become confirmatory rituals, and the office is abolished outright in 1864. Their standing is bound up with the scriptural learning the arrangement enshrines; stepping outside it means abandoning the identity and lineage that learning constitutes.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_intermediaries, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_intermediaries, payer).

% Landholding and scribal castes whose precedence over tenants, laborers, and households receives written, court-enforceable confirmation once the textual hierarchy is treated as the law of the land. They litigate successfully under the codified rules, supply collaborators and witnesses, and lose little; a dominance previously negotiable in village practice now arrives backed by the colonial state.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites, beneficiary,
    powerful, biographical, constrained, regional).

% Bear the codified hierarchy as enforceable law: obligations, exclusions, and punishments previously mediated by negotiable local practice now arrive as fixed textual rules applied by distant courts. Customary spaces that had allowed bargaining, migration, or quiet deviation shrink. Conversion or flight out of the jurisdiction remain formally possible but cost community, livelihood, and standing, and the census follows them with the category regardless.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_and_outcaste_communities, payer,
    powerless, generational, trapped, national).

% Have their inheritance, remarriage, and household standing decided by selected textual rules administered by male pandits and judges. Regional customs that had been more generous in places are overridden by the Sanskrit defaults the translators privileged; the codified rules then harden into the standard against which any later reform must argue.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_subject_to_textual_law, payer,
    powerless, generational, trapped, national).

% Village panchayats, caste councils, and occupational bodies that previously settled most disputes through negotiated, revisable local norms. As the colonial courts assert exclusive cognizance and treat textual law as supreme, these bodies lose jurisdiction, their decisions become voidable at the courthouse, and their accumulated practical knowledge loses official standing.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, customary_village_jurisdictions, payer,
    organized, generational, constrained, local).

% Anti-caste radicals, reformers, and early nationalists who dispute both the content and the provenance of the codified law — Phule's attacks on the Brahmanical codes, later Ambedkar's critique — but who have no seat in the process that defines 'Hindu law' in the first place. They speak only after the categories are fixed, forced to argue inside a frame they did not author.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, anti_caste_and_nationalist_critics, excluded,
    organized, biographical, constrained, national).

% Later historians and philologists — Cohn, Rocher, Dirks, Menski among others — who reconstruct how the corpus was selected, translated, and enforced, and how the 'timeless law' was assembled in the meeting of pandit memory and European philology. They bear none of the arrangement's costs and hold no stake in its continuation; their analyses are the main external check on the participants' self-descriptions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, hermeneutic_scholars_postcolonial, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces an unmanageable plurality of regionally varying, orally transmitted, negotiable customs to a single translatable textual corpus with uniform categories, so that a small colonial cadre can adjudicate disputes, assess revenue, and enumerate the population without ethnographic command of local practice.
% TRANSFER_FUNCTION: Moves interpretive authority over marriage, inheritance, caste obligation, and dispute settlement from living practice communities to the colonial state and its textual intermediaries; moves compliance, revenue legibility, and litigable obligation from colonized subjects to the administration; moves confirmed precedence to landholding upper-caste elites whose position the codified rules entrench.
% ABSENT_VOICES: The governed — above all lower castes, outcaste communities, and women — had no seat where 'their' law was selected and translated; anti-caste radicals and defenders of customary jurisdiction objected publicly only after the categories were fixed. Their absence is what allowed a small circle of judges, translators, and pandit collaborators to speak for an entire civilization's law.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would unravel the Anglo-Hindu court system, the pandit officer corps, the census's caste categories, and the precedent library that post-colonial personal law inherited; adjudication of marriage and inheritance across British India would revert to unsettled pluralism, and the post-independence Hindu Code reforms would have had nothing to reform.
% FOUNDING_PROBLEM: After the East India Company acquired revenue authority in Bengal (1765), it governed millions whose personal laws it did not know, under a declared policy of non-interference in native religion. Its answer was to 'discover' the law in the most authoritative texts, translate them, and administer them — making the corpus into a usable code.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical scholarship of Cohn, Rocher, and Dirks reconstructs the 1772 Hastings plan and its premises from company records; the arrangement's own successors testify against its continuing necessity — the office of Hindu-law pandit was abolished in 1864 as redundant, and the post-independence Hindu Code Acts replaced the administered corpus with enacted statute that the original beneficiaries did not author.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction peaks mid-interval (0.70 around 1900) as census categories, court precedent, and textual defaults interlock, then falls to 0.50 by 1956 as statute displaces the administered corpus. Suppression_requirement tracks enforcement capacity rather than intent: it builds with the court-and-pandit machinery (0.30 to 0.70) and collapses as the apparatus is dismantled (0.40 at end) — the enforcement arc, not a static posture, is the dynamic this story traces. Theater follows the pandit office: genuinely functional at first (0.15), ceremonial once the printed digests decide everything (0.60 by 1900), moot as the office dies (0.35). Accessibility collapse sits mid-range: subjects could not opt out of adjudication under the codified law, but conversion, migration, and jurisdictional friction left real if costly exits. Resistance is substantial and continuous — 1857, the anti-caste movements, reform litigation — because the arrangement had to be actively defended, not merely obeyed. All three series share one seven-point grid (1772-1956) so no metric is sampled against another's gaps; the end-state scalars match the 1956 terminal values by design, since the interval closes on the arrangement's dismantling.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is faithful stewardship: the administration sincerely believes it discovered and applies India's own law, and its self-description (benign transmission, low extraction) is honest at the level of intention. From the payer seats the same structure operates as imposed hierarchy: rules selected by foreign judges and pandit collaborators, enforced by distant courts, overriding customs those seats lived by. The pandit seat straddles the divide — elevated, paid, and finally discarded. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map onto the structure directly: the administration and its legal establishment sit at the beneficiary pole (they collect governability, salaries, and authority); upper-caste elites collect entrenchment; lower castes, women, and the displaced customary jurisdictions sit at the target pole, and their trapped or constrained exits push their effective extraction toward the full-target end. One override is warranted: brahmin_pandit_intermediaries appear in the beneficiaries list, which would derive a near-beneficiary directionality, but the seat was progressively captured — its authority absorbed into translations, its office abolished in 1864 — so an intermediate d (0.38) reflects a collaborator elite that both collected from the arrangement and was consumed by it. The override is keyed to the moderate power atom, which only this seat holds.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — govern millions of unknown law without violating non-interference doctrine — was real and was solved: by the 1860s the administration had its digests, its trained cadre, and its precedent library, and it abolished the pandit office as redundant. The arrangement nonetheless persisted for another ninety years, maintained by inertia and by the constituencies it had created. Reading the structure as pure extraction would erase the genuine coordination achievement (uniform adjudication where none was administrable); reading it as neutral transmission would erase the freezing of hierarchy and the displacement of custom. The hybrid classification keeps both visible, and the dead-founding-problem-plus-world-rearranges combination flags the zombie phase explicitly: the arrangement outlived its function while the world continued to be organized around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading of the kernel vedic_corpus_social_prescription; what would the sibling readings change structurally?',
    'Compare compiled classifications across the three reading-stories: orthodox_varna_reading (divine varna mandate; Brahmin beneficiaries; deeper victim set) and reformist_spiritual_reading (no prescriptive content; no codifiable law; the administrative arrangement dissolves for lack of an object).',
    'Under the orthodox reading the victim set widens and extraction deepens toward enforced divine hierarchy; under the reformist reading the constraint loses its object entirely — there is no unified law to codify — and the arrangement stands exposed as pure administrative construction riding a misread corpus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file is the colonial-orientalist member of a three-reading kernel family.').

omega_variable(
    corpus_unity_constructed,
    'Do the Dharmashastra texts constitute a unified legal system at all, or is ''unity'' an artifact of colonial selection, translation, and digest-making?',
    'Philological comparison of the dharmasutras, smritis, and commentaries for internal contradiction, plus regional manuscript and practice evidence showing how much the digests smoothed over.',
    'If unity is constructed, the coordination function is thinner than claimed and a larger share of measured extraction is attributable to the construction itself rather than to anything the corpus contains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corpus_unity_constructed, empirical, 'Whether textual unity predates colonial codification or was manufactured by it.').

omega_variable(
    pandit_authorship_degree,
    'Were the Brahmin pandit intermediaries co-authors of the colonial construction — steering selection toward texts that entrenched their own office — or instrumentalized informants progressively displaced by it?',
    'Archival study of the Halhed, Jones, and Colebrooke translation correspondence and of the court pandits'' opinion books against the published digests.',
    'Co-authorship pushes the pandit seat''s directionality toward the beneficiary pole despite its late displacement; instrumentalization pushes it toward the target pole and raises measured extraction for the interval before 1864.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pandit_authorship_degree, empirical, 'Degree of pandit agency in shaping the codified corpus.').

omega_variable(
    counterfactual_custom_baseline,
    'How much negotiable flexibility did colonized subjects actually lose when custom was frozen into textual rules — and how much of the frozen content would have hardened anyway without colonization?',
    'Regional comparison of surviving pre-colonial dispute records against post-codification outcomes; the uncolonized counterfactual is unobservable, so resolution is partial at best.',
    'Sets the size of the extraction attributable to freezing versus to the underlying hierarchy; a large autonomous-hardening component lowers this reading''s distinct contribution to the victim set''s condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_custom_baseline, conceptual, 'Counterfactual baseline for the cost of crystallizing fluid practice into fixed law.').

omega_variable(
    rhetorical_sunset_status,
    'The arrangement described itself as provisional administration of indigenous law — a rhetorical sunset — but contained no sunset mechanism; does self-described transitionality make this a scaffold rather than a hybrid coordination/extraction arrangement?',
    'Test for an actual sunset mechanism: a scheduled termination, a defined success condition, or a built-in review that could end the arrangement. None appears in the charters, regulations, or precedent; transition rhetoric recurs but binds nothing.',
    'If rhetorical sunsets counted, the type would shift toward scaffold; as authored — no mechanism, persistence ninety years past the founding problem, enforcement throughout — the structure is a hybrid of genuine coordination and asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_sunset_status, conceptual, 'Why the manifest''s scaffold hypothesis was refined to a hybrid type.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1772, 1956).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcsp_cor_tr_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1772, 0.15).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1772, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1800, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1830, 0.38).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1830, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1864, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1864, 0.55).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1864, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1900, 0.6).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1900, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1930, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1930, 0.55).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1930, observed).
narrative_ontology:measurement(vcsp_cor_tr_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1956, 0.35).
narrative_ontology:measurement_basis(vcsp_cor_tr_t1956, observed).

% Extraction over time
narrative_ontology:measurement(vcsp_cor_be_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1772, 0.45).
narrative_ontology:measurement_basis(vcsp_cor_be_t1772, observed).
narrative_ontology:measurement(vcsp_cor_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement_basis(vcsp_cor_be_t1800, observed).
narrative_ontology:measurement(vcsp_cor_be_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1830, 0.6).
narrative_ontology:measurement_basis(vcsp_cor_be_t1830, observed).
narrative_ontology:measurement(vcsp_cor_be_t1864, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1864, 0.66).
narrative_ontology:measurement_basis(vcsp_cor_be_t1864, observed).
narrative_ontology:measurement(vcsp_cor_be_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement_basis(vcsp_cor_be_t1900, observed).
narrative_ontology:measurement(vcsp_cor_be_t1930, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1930, 0.68).
narrative_ontology:measurement_basis(vcsp_cor_be_t1930, observed).
narrative_ontology:measurement(vcsp_cor_be_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1956, 0.5).
narrative_ontology:measurement_basis(vcsp_cor_be_t1956, observed).

% Suppression requirement over time
narrative_ontology:measurement(vcsp_cor_su_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1772, 0.3).
narrative_ontology:measurement_basis(vcsp_cor_su_t1772, observed).
narrative_ontology:measurement(vcsp_cor_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement_basis(vcsp_cor_su_t1800, observed).
narrative_ontology:measurement(vcsp_cor_su_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1830, 0.55).
narrative_ontology:measurement_basis(vcsp_cor_su_t1830, observed).
narrative_ontology:measurement(vcsp_cor_su_t1864, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1864, 0.65).
narrative_ontology:measurement_basis(vcsp_cor_su_t1864, observed).
narrative_ontology:measurement(vcsp_cor_su_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement_basis(vcsp_cor_su_t1900, observed).
narrative_ontology:measurement(vcsp_cor_su_t1930, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement_basis(vcsp_cor_su_t1930, observed).
narrative_ontology:measurement(vcsp_cor_su_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1956, 0.4).
narrative_ontology:measurement_basis(vcsp_cor_su_t1956, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_caste_categories).

% DUAL FORMULATION NOTE:
% The colloquial label 'Vedic social prescription' decomposes, per the ε-invariance principle, into three structurally distinct readings of one kernel: this colonial-orientalist story (ε rising 0.45 to 0.70 across its administrative life; beneficiary: colonial administration; victims: codified subjects), the orthodox varna reading (divine-mandate claim; different beneficiary and victim sets), and the reformist spiritual reading (no prescriptive content; no codification object). Each is a separate file with its own ε, stakeholders, and classification; the files cross-link through affects_constraints. This reading also structurally feeds the census-classification apparatus (colonial_census_caste_categories), which inherits its categories and extends their fixity beyond the legal domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
