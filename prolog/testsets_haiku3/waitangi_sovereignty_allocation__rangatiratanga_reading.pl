% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi Article II: Māori Tino Rangatiratanga (Full Authority) Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is the founding document of the New Zealand
 *   state. The English Article I states the Crown received 'sovereignty' from
 *   Māori chiefs; the Māori Article II states Māori retained 'tino
 *   rangatiratanga' (full/absolute authority) over their lands, villages, and
 *   treasures, and the Crown gained only 'kāwanatanga' (governorship) over
 *   settlers. This reading instantiates the Māori-language Article II
 *   interpretation: Māori possess inherent authority (tino rangatiratanga)
 *   over traditional territories and resources; the Crown's authority is
 *   limited to governance of settler populations (kāwanatanga). This reading
 *   has been partially operationalized through the Treaty of Waitangi Act
 *   1975, Waitangi Tribunal claim settlements, and recent co-governance
 *   frameworks, yet the Crown still legally claims ultimate legislative
 *   sovereignty through Parliament. The constraint measures the gap between
 *   Māori asserted tino rangatiratanga and the Crown-permissible exercise of
 *   Māori authority within a system that ultimately subordinates it to
 *   Westminster supremacy. KEY AGENTS (by structural relationship): Māori iwi
 *   and hapū (organized beneficiaries, identity-locked, the authority holders
 *   under this reading); Crown state and Parliament (institutional
 *   agenda-setters, constrained payers, ultimately claiming overrideable
 *   authority); settler populations (powerful payers, mobile, constrained by
 *   Māori authority in Māori territories); courts (observers and
 *   interpreters, mediating between readings); historic settler colonial
 *   institutions (excluded, would argue for crown_sovereignty_reading).
 *
 * KEY AGENTS:
 *   - Māori iwi and hapū — primary beneficiaries of tino rangatiratanga recognition; identity-locked to territories and governance roles; organized political actors
 *   - Crown state (executive, legislature, administration) — agenda-setter maintaining institutional structure; payer in surrendered claims to absolute sovereignty; arbitrage exit (could repudiate treaty but would delegitimize itself)
 *   - Crown Parliament — claims ultimate legislative power; constrained by treaty interpretation and public/international pressure; institutional time horizon
 *   - Settler-descended New Zealand population — powerful institutional actors; pay through resource access restrictions and governance subordination in Māori territories; mobile exit
 *   - Courts and Waitangi Tribunal — observers and enforcers; interpret the treaty and apply readings to resolve disputes; have moved toward recognizing rangatiratanga claims
 *   - International indigenous rights regime — vindicated non-agent beneficiary; alignment with UN DRIP and ILO 169 legitimizes the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.72).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Article II: Māori Tino Rangatiratanga (Full Authority) Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'a14088c3-0c54-494e-bfed-ba955ad6cd09').
narrative_ontology:cs_kernel_codification('a14088c3-0c54-494e-bfed-ba955ad6cd09', fixed_text).
narrative_ontology:cs_authority_grounding('a14088c3-0c54-494e-bfed-ba955ad6cd09', lineage).
narrative_ontology:cs_interpretation_layer_present('a14088c3-0c54-494e-bfed-ba955ad6cd09').
narrative_ontology:cs_reading_relation('a14088c3-0c54-494e-bfed-ba955ad6cd09', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('a14088c3-0c54-494e-bfed-ba955ad6cd09', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('a14088c3-0c54-494e-bfed-ba955ad6cd09', foundational, maori_tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(maori_tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('a14088c3-0c54-494e-bfed-ba955ad6cd09', maori_tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('a14088c3-0c54-494e-bfed-ba955ad6cd09', foundational, crown_kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(crown_kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('a14088c3-0c54-494e-bfed-ba955ad6cd09', crown_kawanatanga_limited_to_settlers, deontological).
narrative_ontology:cs_reference_frame('a14088c3-0c54-494e-bfed-ba955ad6cd09', maori_inherent_tino_rangatiratanga).
narrative_ontology:cs_drift_state('a14088c3-0c54-494e-bfed-ba955ad6cd09', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a14088c3-0c54-494e-bfed-ba955ad6cd09', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_interests_through_crown_usurpation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_state).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settlers_and_non_maori).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, iwi and hapū retain inherent authority (tino rangatiratanga) over their traditional lands, resources, and taonga (treasures/cultural property). They are the decision-makers in their territories and stewards of their own governance structures. The constraint binds their identity and ancestral relationship to the land; exit is conceptually impossible — they cannot abandon the territories or the authority relationships that constitute their legitimacy as iwi.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary).

% Membership in Māori communities positioned to exercise self-determination and control resource allocation decisions affecting their welfare. The rangatiratanga reading asserts their right to make binding decisions about land use, resource extraction, cultural practice, and governance within their territories. Constrained exit because community membership and cultural participation are partially identity-fused; moving away reduces but does not eliminate the identity tie.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities, beneficiary,
    moderate, generational, constrained, national).

% Under this reading, the Crown holds only kāwanatanga (governorship) over settler populations and Crown-administered territories, not tino rangatiratanga. The Crown can exercise executive and legislative authority where it has jurisdiction, but that jurisdiction is limited by the retention of Māori authority in Māori territories. The Crown pays by surrendering claims to absolute sovereignty and by the burden of negotiating governance arrangements with Māori authorities rather than imposing them unilaterally.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_state, payer).

% Non-Māori settlers and populations under Crown jurisdiction may face restrictions on resource access, land use, or governance participation in Māori territories where Māori authorities exercise tino rangatiratanga. Their exit option is mobility — they can relocate to Crown-controlled areas or to territories where they hold recognized authority; they are not locked into Māori territories by identity or ancestral ties.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settlers_and_non_maori, payer,
    powerful, biographical, mobile, national).

% Westminster-style parliament claims plenary legislative power under the competing crown_sovereignty_reading. Under the rangatiratanga reading, parliament's power is constrained by the Treaty and by Māori retained authority; the constraint limits parliament's ability to unilaterally legislate in domains where Māori tino rangatiratanga applies. Parliament could theoretically exit by repudiating the Treaty, but doing so would violate its own foundational legitimacy claim (rule of law grounded in solemn commitments).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% The judicial system interprets the Treaty of Waitangi and applies it to resolve conflicts between Crown authority and Māori claims. Courts have moved toward recognizing substantive Māori rights based on the rangatiratanga reading, particularly since the Treaty of Waitangi Act 1975 and the Waitangi Tribunal. They observe the structural conflict between competing readings and apply the constraint through doctrine.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, courts_and_judicial_system, observer,
    institutional, generational, analytical, national).

% Historic settler institutions — early Crown land purchase schemes, resource extraction monopolies, legislative supremacy doctrines — are structurally excluded from benefiting under the rangatiratanga reading. These institutions would argue for Crown plenary power; they are locked out of the conversation by the Treaty's terms (as read by this reading) and by Māori assertions of retained authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_colonial_institutions, excluded,
    institutional, generational, trapped, national).

% The rangatiratanga reading aligns with and is vindicated by international norms on indigenous self-determination (UN Declaration on the Rights of Indigenous Peoples, ILO Convention 169) that recognize inherent indigenous sovereignty and free prior informed consent. The doctrine/norm is a non-agent beneficiary: it is strengthened and legitimized when Māori tino rangatiratanga is recognized, but it collects no rents.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_indigenous_rights_regime, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__rangatiratanga_reading, international_indigenous_rights_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, diffuse).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a structured coexistence of two authority systems: Māori tino rangatiratanga (inherent authority over traditional territories and resources) and Crown kāwanatanga (administrative governance over settler populations and Crown-administered domains). Solves the coordination problem of allocating sovereign authority between two polities with overlapping territorial claims without requiring one to subordinate the other absolutely.
% TRANSFER_FUNCTION: Transfers authority from Crown claims of absolute sovereignty to Māori assertions of retained inherent authority. The transfer is backwards-looking (restores what was retained, not a new concession) in the Māori reading; it is a constraint on Crown power rather than a positive grant to Māori from Crown. What flows is deference, recognition, and jurisdictional space — not money or resources directly, but the authority to control resources and make governance decisions.
% ABSENT_VOICES: Settler colonial institutions and early non-Māori settlers who expected uncontested Crown sovereignty are excluded from authorizing this reading; they would argue for the crown_sovereignty_reading instead. Their voices were not in the Māori-language treaty conversation and are structurally sidelined by this reading's logic. Historic Crown legal advisors and Westminster supremacists remain in the conversation through parliament and courts, but under a constraint, not as authoritative narrators.
% DISAPPEARANCE_RATIONALE: If this reading and the Māori authority structure it recognizes were to disappear, governance over vast territories would revert entirely to Crown/parliamentary control; Māori resource management authority would evaporate; land claims and co-governance agreements would lose their constitutional foundation and could be unilaterally revoked. The political, legal, and institutional arrangements of modern Aotearoa New Zealand are structured around this reading's partial recognition — its disappearance would require massive institutional reorganization and would trigger Māori resistance and constitutional crisis.
% FOUNDING_PROBLEM: The Treaty of Waitangi (1840) required allocation of sovereignty between Crown and Māori in a way that both parties could claim legitimacy. The English Article I stated the Crown received sovereignty; the Māori Article II stated Māori retained tino rangatiratanga. The founding problem is the irreconcilable textual gap: do the two articles describe the same allocation (in which case one reading is false) or does 'sovereignty' mean different things in each language (ceding Crown authority over settlers vs. retaining Māori inherent authority)? This reading resolves it by asserting that 'kāwanatanga' and 'tino rangatiratanga' are genuinely distinct concepts, not mistranslations.
% FOUNDING_PROBLEM_CORROBORATION: Māori historians, linguists, and iwi leaders attest that the Māori text Article II was understood by Māori signatories as retaining authority (tino rangatiratanga means absolute/full authority). The Waitangi Tribunal (established 1975) has endorsed this reading in numerous reports and claims assessments. International indigenous rights bodies and comparative constitutional scholars (e.g., James Belich, Anne Salmond on translation history) corroborate the linguistic and historical basis. Crown apologists and Westminster supremacists counter that the Crown always intended absolute sovereignty and that Māori understanding was mistaken or tactical — their corroboration is internal to Crown institutional interests and does not stand independent of the benefiting parties.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) measures the gap between Māori asserted tino rangatiratanga and actual Crown-permissible authority in 2024. The Māori text Article II claims full authority; the Crown recognizes this claim in rhetoric but retains ultimate legislative override in law and practice. The measurement series (0.92 → 0.68) traces the long suppression of Māori authority (1840–1975) followed by gradual, incomplete restoration (1975–2024). Extractiveness decreases as Māori governance authority is recognized and operationalized in co-governance frameworks, but the Crown's retained claim to ultimate sovereignty keeps extractiveness substantial — the constraint is structurally extractive because Māori cannot enforce tino rangatiratanga against parliamentary legislation. Suppression (0.72) measures the active enforcement needed to maintain Crown legislative supremacy while appearing to respect the treaty. Historic suppression was military and administrative (1840–1975, values 0.95 → 0.81); contemporary suppression is structural and legal (courts upholding parliamentary supremacy, co-governance frameworks with Crown veto). Theater_ratio (0.58) is high because the 'partnership' and 'bicultural' framing of contemporary Crown-Māori relations exceeds the actual power-sharing — co-governance is designed with Crown final-say authority, and the nation brands itself as respecting tino rangatiratanga while Parliament retains unilateral power. Accessibility_collapse (0.45) is moderate because alternatives to this reading remain live: the crown_sovereignty_reading still has institutional support (Parliament, traditional common law), and the partnership_reading offers a middle path. Resistance (0.81) is high because Māori communities actively assert tino rangatiratanga claims, challenge Crown overreach in courts, and mobilize politically for recognition — the constraint is not passively accepted. The Claimed_type (tangled_rope) reflects genuine coordination function (two authority systems coexisting) + asymmetric extraction (Crown retains veto/override, Māori authority is delegated/contingent). This is not pure snare because real coordination is required — two polities must share territory. It is tangled rope because the coordination is nested in extraction: Māori benefit from recognized governance authority, but the Crown extracts the ability to override it.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and hapū are structural beneficiaries of tino rangatiratanga recognition (they accrue the authority and can exercise it in practice), but they are also structurally targeted by the Crown's retained override capability — directionality for Māori seats is high (d → 0.7–0.85) because they bear the risk that Crown can unilaterally revoke or restrict their authority. Māori communities are beneficiaries and constrained payers. Crown state is an agenda-setter collecting the benefit of retained ultimate sovereignty (d → 0.2–0.3, near beneficiary end, though it pays in legitimacy) while paying in surrendered claims to absolute authority and constrained governance space. Settler populations are payers (restricted access in Māori territories) with mobile exit, so d → 0.55–0.70. Courts are observers with no stake in the authority allocation itself (d → 0.5, analytical). The asymmetry between Māori identity-locked positions and Crown/settler mobile positions is the structural engine of extraction: those who cannot leave pay more than those who can. Directionality overrides are not needed here; the derived d from beneficiary/victim + exit_options captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem: the 1840 allocation of sovereignty between Crown and Māori was textually ambiguous (English Article I vs. Māori Article II) and operationally violated immediately by the Crown's assertion of plenary sovereignty and military suppression of Māori authority. Founding problem status: contested — the Crown and its successors claim the ambiguity was resolved in the Crown's favor, while Māori and indigenous rights advocates claim the rangatiratanga_reading is the true allocation and the Crown violated it from the start. Disappearance verdict: world_rearranges — if this reading and the Māori authority structures it recognizes were to vanish, New Zealand's entire constitutional arrangement (co-governance, Treaty settlements, bicultural policy) would require fundamental reorganization; Māori resistance would escalate; international indigenous rights commitments would be breached. Mandatrophy check: The founding problem (sovereignty allocation) is NOT dead — it remains highly contested and the subject of ongoing political and legal struggle. The constraint persists because both parties continue to invoke it, not because the founding problem is obsolete. Māori use the rangatiratanga_reading to claim authority; the Crown uses it (reinterpreted as 'consultation rights' or 'partnership') to justify co-governance without surrendering parliamentary supremacy. The constraint is NOT mandatropic by this analysis — it is actively maintained and reinterpreted, not theatrically performed while the function is forgotten. Theater_ratio at 0.58 (not negligible) indicates performative elements in how the Crown frames partnership, but the constraint's core function (allocating authority between two polities) remains substantively operational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rangatiratanga_vs_sovereignty_semantics,
    'Does ''tino rangatiratanga'' in the Māori Article II genuinely mean absolute inherent authority over territories and resources, or does it mean something closer to ''respect and consultation rights'' compatible with ultimate Crown sovereignty?',
    'Linguistic analysis of 19th-century Māori usage (historical sources, contemporary Māori understanding as recorded), comparative examination of how tino rangatiratanga is used in other Māori-language texts from the same period, ethnographic evidence of Māori leadership concepts at the time of signing.',
    'If ''tino rangatiratanga'' genuinely means absolute authority, this reading is structurally sound and the measured extractiveness reflects Crown usurpation of retained Māori authority. If it is weaker (e.g., advisory/consultation rights), the reading''s claim of inherent Māori sovereignty is overclaimed and the constraint should be reclassified as rope (genuine coordination) rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_vs_sovereignty_semantics, empirical, 'Semantic scope of the Treaty''s tino rangatiratanga commitment').

omega_variable(
    maori_authority_exercise_modality,
    'In the absence of Crown legislative supremacy, how would Māori tino rangatiratanga be exercised in practice across diverse iwi with different governance traditions? Is there a coherent institutional model or does ''tino rangatiratanga'' require extensive negotiation over implementation?',
    'Review of contemporary Māori governance proposals (iwi constitutions, co-governance frameworks), legal analysis of how other jurisdictions implement indigenous authority (Canada, Australia, US), pilot implementations of substantive co-governance (e.g., DOC co-management agreements) and their operational outcomes.',
    'If tino rangatiratanga has a coherent and workable operational model, the reading''s claim to be a stable alternative constitutional settlement is stronger. If implementation is radically context-dependent and fragmented, the reading may require substantially more institutional work than currently acknowledged; the claim becomes conditional on institutions not yet designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maori_authority_exercise_modality, conceptual, 'Institutional viability of tino rangatiratanga governance structures').

omega_variable(
    crown_sovereignty_claim_legitimacy,
    'Can the Crown''s claim to legislative supremacy (ultimately overrideable Parliament) coexist with genuine recognition of Māori tino rangatiratanga, or does one reading foreclose the other?',
    'Constitutional law analysis of whether ''tino rangatiratanga'' can be subordinate to Crown parliament without becoming an advisory fiction; comparison with federalism models (US, Canada, Australia) where sub-sovereign units retain genuine authority over specific domains; analysis of whether parliamentary supremacy + bounded Māori authority is a stable equilibrium or a transition state toward full separation.',
    'If they can coexist stably (true federalism/co-sovereignty), the rangatiratanga reading and the crown_sovereignty reading COEXIST_WITH each other (parliamentary supremacy in Crown domains, Māori authority in Māori domains). If parliamentary supremacy is truly unlimited, the readings FORECLOSE each other — one reading must win and the other must lose. The coexistence_with or forecloses relation in cs_structure.reading_relations depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_sovereignty_claim_legitimacy, conceptual, 'Logical compatibility of tino rangatiratanga and Crown parliamentary supremacy').

omega_variable(
    identity_lock_vs_exit,
    'How deeply identity-locked are Māori iwi and communities to their territories and governance roles such that exit from the rangatiratanga reading is psychologically/spiritually impossible rather than merely constrained?',
    'Ethnographic research on Māori concepts of belonging (whakapapa, mana, whenua), post-settlement outcomes for Māori who have relocated or assimilated, analysis of whether Māori identity can persist outside of territorial governance relationships, case studies of identity-locked vs. identity-mobile positions within Māori communities.',
    'High identity-lock (exit = identity death) supports the classification of Māori exit_options as identity_locked and increases the structural asymmetry of the constraint — Māori cannot exit but Crown/settlers can. Lower identity-lock would shift some Māori positions toward constrained or even mobile exit, reducing the effective extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_exit, empirical, 'Depth of identity-fusion binding Māori to territorial governance').

omega_variable(
    kernel_reading_contest_framing,
    'Is the Treaty of Waitangi properly understood as a kernel with multiple readings of a single text (interpretation contest), or is it better understood as a flawed translation where one reading is true and the others are factual errors?',
    'Historical analysis of whether the two-text problem (English Article I vs. Māori Article II) was known to signatories at the time or discovered later; ethnographic evidence of whether Māori signatories and Crown representatives understood they were agreeing to different things; examination of how similar translation gaps have been resolved in other founding documents.',
    'If the framers were aware of the semantic gap and chose it deliberately or allowed it to stand, the kernel framing is correct (multiple readings coexist). If the gap is a later discovery of a mistranslation, the readings do not all hold equally — one reading is the mistaken one and the others correct it. The epistemological status of the kernel affects whether cs_structure.reading_relations should show coexists_with or forecloses between the rangatiratanga_reading and its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, empirical, 'Kernel vs. mistranslation status of the two-text Treaty problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(wait_be_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 23, 0.88).
narrative_ontology:measurement(wait_be_t46, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 46, 0.84).
narrative_ontology:measurement(wait_be_t92, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 92, 0.78).
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wait_be_t158, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 158, 0.42).
narrative_ontology:measurement(wait_be_t174, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 174, 0.38).
narrative_ontology:measurement(wait_be_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 184, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(waitangi_sovereignty_allocation__rangatiratanga_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.15).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel decomposes into three readings with structurally distinct ε values and victim/beneficiary sets. The rangatiratanga_reading (this file) measures extractiveness as the gap between Māori asserted tino rangatiratanga and Crown-permissible authority (ε ≈ 0.68). The crown_sovereignty_reading would measure extractiveness from the Crown's perspective (ε ≈ 0.05, no extraction from Crown's logic — Crown acts as it claims to have authority to act). The partnership_reading measures extractiveness as the breach of partnership obligations (ε ≈ 0.40–0.50, moderate extraction). All three are stories about the same kernel; they coexist as live institutional and political positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
