% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Latin Script Adoption as Secular-Modernist State Foundation (Kemalist Reading)
 *   domain: political/linguistic/identity
 *
 * SUMMARY:
 *   Between 1928 and 1935, the Turkish nationalist state under Atatürk
 *   mandated replacement of Arabic script with Latin script for the Turkish
 *   language—a symbolic and practical rupture from Ottoman-Islamic tradition
 *   presented as necessary for modernization and European alignment. This
 *   story instantiates ONE READING of the contested kernel 'script as
 *   identity': the Kemalist rupture reading, which frames the transition as
 *   enabling secular modernity by severing institutional continuity with the
 *   Islamic past. The constraint is CLAIMED as tangled_rope (it coordinates
 *   literacy and breaks with backwardness while extracting authority from
 *   religious institutions) while the authored metrics describe high
 *   extractiveness (0.62), substantial suppression (0.71 at steady state,
 *   rising to 0.71), and moderate theatricality (0.48 rising from 0.22). The
 *   claim-metric divergence is deliberate: this reading's beneficiaries
 *   (state, urban elite, modernist intellectuals) experience the constraint
 *   as necessary and coordinating; the victims (Islamic literati, rural
 *   communities, Sufi networks) experience it as coercive erasure. The
 *   engine's per-seat classification will capture this divergence from the
 *   structural data—no reconciliation by the author.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: institutional agenda-setter, monopolizes enforcement through education and printing control, benefits from suppression of alternative authority structures
 *   - urban_educated_elite: powerful beneficiaries with high exit options (arbitrage), already comfortable with Latin script from European education, face zero literacy transition cost
 *   - arabic_script_literati: moderate-power payers with identity-locked exit, professional expertise rendered valueless overnight, cannot retrain without abandoning identity
 *   - rural_religious_communities: powerless payers, trapped, literacy severed from inherited religious knowledge and transmission chains
 *   - sufi_educational_networks: moderate-power payers with constrained exit, alternative educational authority structures targeted by enforcement
 *   - printing_and_publishing_sector: organized beneficiaries, capture market demand from transition and benefit from suppression of Arabic-script competitors
 *   - ottoman_religious_courts: moderate-power payers, institutional legitimacy stripped by state centralization and script transition
 *   - phonetic_instrumentalism_advocates: explicitly excluded from official narrative, would argue for script reform on technical grounds without ideological rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.62).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.71).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Latin Script Adoption as Secular-Modernist State Foundation (Kemalist Reading)").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/linguistic/identity").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'a7a5b6c0-1640-4c99-a617-2ba52b2b119d').
narrative_ontology:cs_kernel_codification('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', formalized).
narrative_ontology:cs_authority_grounding('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', extraction).
narrative_ontology:cs_interpretation_layer_present('a7a5b6c0-1640-4c99-a617-2ba52b2b119d').
narrative_ontology:cs_reading_relation('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', foundational, textual_rupture_enables_modernity).
narrative_ontology:cs_axiom_status(textual_rupture_enables_modernity, holdable).
narrative_ontology:cs_axiom_grounding('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', textual_rupture_enables_modernity, instrumental).
narrative_ontology:cs_axiom('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', foundational, state_monopoly_is_coordinating).
narrative_ontology:cs_axiom_status(state_monopoly_is_coordinating, holdable).
narrative_ontology:cs_axiom_grounding('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', state_monopoly_is_coordinating, instrumental).
narrative_ontology:cs_reference_frame('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', ottoman_islamic_intellectual_continuity).
narrative_ontology:cs_drift_state('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', post_kemalist_reformation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('a7a5b6c0-1640-4c99-a617-2ba52b2b119d', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_nationalist_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, urban_educated_elite).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_literati).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_religious_communities).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, sufi_educational_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, rural_religious_communities).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, printing_and_publishing_sector).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, rural_literacy_campaign_participants).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, european_aligned_intellectuals).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, state_literacy_apparatus).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_religious_courts).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_literacy_campaign_participants).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_calligraphy_masters).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, rupture_with_ottoman_past_is_modernization).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secular_nationalism_requires_textual_break).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees script adoption as central to national transformation. Controls education curriculum, literacy campaigns, and printing. Frames the transition as unavoidable progress: Latin script enables phonetic transparency, removes Ottoman-Islamic institutional drag, and signals unambiguous break with empire. Suppresses Arabic script literacy instruction and delegitimizes Islamic scholarly networks. Benefits from the constraint by consolidating state monopoly over cultural meaning-making and blocking alternative authority structures (religious institutions, Ottoman-trained ulama).
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Professionals, military officers, bureaucrats educated in Europe or European-influenced institutions. Already comfortable with Latin script from foreign travel and education. Benefit from script adoption because it aligns their foreign training with national prestige, secures their role as mediators between Turkish masses and European modernity, and positions them as guardians of the transition. Face no literacy transition cost; exit options are high (they can operate in both scripts). Directly benefit from the constraint's suppression of Arabic-script authority structures.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, urban_educated_elite, beneficiary,
    powerful, biographical, arbitrage, national).

% Islamic scholars, calligraphers, legal scholars trained in Ottoman madrasas. Their entire professional identity and legitimacy rest on mastery of Arabic script, Islamic jurisprudence, and Ottoman-Turkish literary tradition. The transition renders their expertise valueless overnight: their books become unreadable to the new generation, their interpretive authority is displaced, and they cannot retrain. They are not displaced by economic competition but by state fiat. Exit would mean abandoning professional identity; staying means becoming obsolete. Moderate power derives from traditional respect and institutional positions in waqfs and religious courts, but these are stripped by the constraint itself.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_literati, payer,
    moderate, biographical, identity_locked, national).

% Village populations dependent on Islamic religious instruction, Quranic recitation, and oral transmission of Islamic practice. Their literacy (where it exists) is typically Arabic script only. The constraint cuts them off from inherited religious texts and severs the transmission chain to younger generations who learn Latin script in state schools. They are trapped: no alternative education system, no choice in curriculum, no geographical exit. The constraint's formal justification (phonetic transparency) has zero relevance to their experience; they experience it as cultural erasure. Simultaneously, many benefit from state education infrastructure now reaching villages, though at the cost of religious content displacement.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_religious_communities, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, rural_religious_communities, beneficiary).

% Sufi lodges (zaviyes, tekkes) operated informal education, spiritual transmission, and social welfare functions outside state control. Their curriculum centered on Quranic interpretation, Hadith study, and mystical texts in Arabic and Ottoman Turkish written in Arabic script. The constraint forces them to either close (many are banned outright) or adapt by abandoning their traditional texts. Constrained exit: they cannot freely operate in competition with state schools, cannot legally teach Arabic script, cannot transmit their inherited knowledge. They do not directly serve state modernization but are collateral damage to the constraint's enforcement against alternative authority structures.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, sufi_educational_networks, payer,
    moderate, generational, constrained, national).

% Printers, publishers, and literacy educators positioned to benefit from the massive demand for new textbooks, Latin-script typefaces, and literacy campaigns. They operate printing houses and establish new publishing standards. The state's enforcement of the transition creates a captive market for their services. They benefit from the constraint's enforcement because it prevents competing publishers from using Arabic script and creates artificial scarcity in the transition period. Relatively mobile exit (they can print in other languages or relocate if conditions worsen) but strongly incentivized to support the constraint.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, printing_and_publishing_sector, beneficiary,
    organized, biographical, mobile, national).

% Islamic law courts that operated under Ottoman rule using Arabic script for legal judgments and Quranic citation. The transition renders their legal documentation illegible to the new generation and strips their authority by nationalizing and secularizing the legal system. Constrained exit: they cannot operate outside state supervision, cannot appeal to scriptural authority in a Latin-script legal framework, and are gradually replaced by secular courts. The constraint directly targets their institutional legitimacy.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_religious_courts, payer,
    moderate, biographical, constrained, national).

% Children and adults in literacy programs introduced to Latin script. They benefit from access to formal education and reading ability in the dominant script. However, they pay through the loss of continuity with their parents' knowledge and religious tradition, and through the state's monopoly control of what gets taught via the new literacy. Trapped: no alternative education, no choice in script, no opt-out from participation in the modernization narrative.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_literacy_campaign_participants, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, rural_literacy_campaign_participants, payer).

% Turkish intellectuals, linguists, and nationalists who frame the script transition as proof of European-style progress and rational language reform. They benefit by having their worldview validated as inevitable and modern. They have high exit options (can publish internationally, emigrate if needed). They directly benefit from the suppression of alternative readings (ottoman_continuity_reading, phonetic_instrumentalism_reading) by claiming the constraint serves self-evident modernization, not ideology.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, european_aligned_intellectuals, beneficiary,
    powerful, biographical, arbitrage, national).

% Specialized artisans and artists whose entire craft is calligraphic excellence in Arabic script. They cannot adapt their skill to Latin typography because calligraphy is not a valued aesthetic in the new regime. Identity-locked: their identity as master calligraphers is severed. Some emigrate; others become obsolete. The constraint does not compete economically with their services but delegitimizes the tradition they embody.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_calligraphy_masters, payer,
    moderate, biographical, identity_locked, national).

% The newly created Ministry of Education and state printing authority that monopolizes textbook production, teacher training, and curriculum. Benefits from the constraint because it grants unquestioned authority over the definition of literacy and meaning-making. Enforces the transition by controlling schools, suppressing Arabic-script materials, and training teachers exclusively in Latin script pedagogy. The constraint's active enforcement exists primarily in the education apparatus's control mechanisms.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, state_literacy_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, state_literacy_apparatus, beneficiary).

% Linguists and scholars who argue for script change on purely phonetic grounds (Latin is superior for Turkish vowel harmony) but who do not endorse rupture with Ottoman past or secular-nationalist ideology. They would argue for pragmatic script reform without cultural delegitimization. Excluded from the framing because the Kemalist reading subordinates phonetics to ideology—the transformation is framed as civilizational rupture, not technical improvement. Their exclusion from the official narrative is itself a constraint effect.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified national literacy system: a single script enables state-controlled, standardized education reaching the entire population. Coordination problem: in a multi-script empire, literacy fragmented along religious and educational lines (Quranic schools, madrasas, European-educated elites). A single dominant script theoretically solves distribution and mutual comprehensibility by fiat.
% TRANSFER_FUNCTION: Transfers authority over meaning-making from decentralized religious institutions and Ottoman-trained intellectuals to the secular nationalist state. Transfers literacy instruction monopoly to state schools. Transfers cultural legitimacy from Ottoman-Islamic tradition to European-aligned modernity. The payers (religious literati, rural communities, alternative intellectual networks) bear the cost of severed knowledge transmission; beneficiaries (state apparatus, urban elite, modernist intellectuals) capture unquestioned authority.
% ABSENT_VOICES: Ottoman-trained Islamic scholars excluded by design—they would attest that the constraint severs a coherent 1,000-year intellectual tradition and that phonetic improvement can occur within Arabic script. Sufi teachers would testify that the constraint is not enabling literacy but eliminating alternative authority structures. Rural communities subject to the constraint have no formal voice in education policy; their resistance is registered as 'backwardness' rather than legitimate objection. Phonetic instrumentalists (who might support script change on technical grounds) are excluded because admitting their reasoning would separate script reform from ideological rupture.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if Latin script were no longer enforced and Arabic script literacy were restored to equal standing—the educational landscape would reorganize: parallel literacy systems would re-emerge, religious institutions would reconstitute scholarly networks using Arabic script materials, Ottoman-era texts would regain accessibility, and alternative intellectual authority structures would no longer be delegitimized by script invisibility. The state's monopoly on cultural meaning-making would weaken. This reorganization is precisely what the constraint's enforcement prevents.
% FOUNDING_PROBLEM: Ottoman empire was fragmenting; Islamic institutional networks were perceived by nationalist elites as backward and anti-modern; European powers were dominating; Turkey needed rapid, visible rupture with imperial past to establish a new national identity distinct from Ottoman-Islamic tradition and legible to European powers as 'modern.' Script change was chosen as a highly visible, irreversible signal of this break.
% FOUNDING_PROBLEM_CORROBORATION: Kemalist officials and modernist intellectuals testify the founding problem (Ottoman backwardness, need for civilizational rupture) remains live. Ottoman scholars and religious communities testify the problem is a constructed narrative—Ottoman institutions were functional, Islamic learning was sophisticated, and the 'problem' is that elites wanted power over meaning-making. International scholars document the constraint was motivated more by nationalist ideology than by literacy necessity. No voice outside the benefiting elite corroborates that script change was the *necessary* solution; alternative framings (gradual reform, pluralism, phonetic improvement without rupture) were available but rejected.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.35 (early transition) to 0.62 (steady state) as the constraint's true function becomes clear: it is not merely phonetic improvement but consolidation of state monopoly over cultural meaning-making. Early extractiveness is lower because the initial transition could plausibly be framed as pure coordination (unified literacy). By year 20, extractiveness stabilizes at 0.62 because the state has successfully suppressed alternative frameworks and the constraint is now self-perpetuating through educational indoctrination. Suppression rises from 0.45 to 0.71 because active coercion is required early (banning Arabic script printing, closing madrasas, firing teachers) but can decline once the new generation knows only Latin script—the suppression requirement stays high (0.71) because enforcement infrastructure must be maintained to prevent re-emergence of Arabic script use or alternative educational networks. Theater ratio rises from 0.22 to 0.48 because the constraint's performative function increases: by year 20, the phonetic justification is largely achieved, but enforcement continues to serve the ideological function of performing 'rupture with backwardness.' The plateau at t=30-40 reflects stabilization: the constraint has achieved its structural goal (state authority monopoly, knowledge transmission severed from Islamic past) and maintenance becomes routine, though theatrical enforcement continues. The measurements use a shared time grid (every metric authored at every time point) to avoid OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The kemalist_state_apparatus and urban_educated_elite see the constraint as enabling necessary modernization with negligible cost (they have no literacy transition burden). The engine will compute their directionality near 0.0-0.3 (beneficiary end), and their seat will classify as rope or scaffold (coordination function, minimal extraction from their perspective). Arabic_script_literati and rural_religious_communities experience the constraint as coercive erasure with zero offsetting coordination benefit (the 'coordination' it provides is not their problem; they already have literacy systems). The engine will compute their directionality near 0.8-1.0 (target end), and their seats will classify as snare (extraction with suppression, no real coordination). This per-seat divergence from the story-level claim (tangled_rope) is exactly what the framework is designed to detect—the constraint is simultaneously rope-like and snare-like depending on which seat you occupy. The structural asymmetry is: beneficiaries pay zero transition cost and benefit from suppression of competitors; victims pay maximal transition cost (livelihood, identity, knowledge transmission) and receive no offsetting coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: kemalist_state_apparatus and urban_educated_elite have d near 0.0-0.15 (full beneficiary end). They collect rents (authority monopoly, unquestioned cultural legitimacy, European alignment), face zero transition costs (already Latin-script literate), and have high exit options (they can publish internationally, emigrate, operate in hidden scripts if needed). The constraint actively suppresses their competitors (Arabic-script intellectuals, religious networks). Victim directionality: arabic_script_literati have d near 0.95 (near full target). They are identity-locked (professional identity inseparable from script mastery), face maximal transition cost (overnight obsolescence), and cannot exit without abandoning identity and career. Rural communities have d near 0.9 (near full target): trapped exit, zero offsetting coordination benefit, knowledge transmission severed. Excluded phonetic_instrumentalism advocates have intermediate d (~0.6-0.7): constrained exit, their legitimate technical arguments are suppressed, but they are not directly victimized (they could in principle support script reform on phonetic grounds if the ideological framing were removed). The overrides are not needed here: the structural derivation from beneficiary/victim declarations and exit options correctly predicts the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman backwardness, need for rupture with Islamic past) is declared as live (founding_problem_status: contested). However, the disappearance_verdict is world_rearranges, which indicates that the constraint's function is not natural or inevitable but depends on active maintenance and beneficiary interest. The mismatch is: founding_problem_status = contested + disappearance_verdict = world_rearranges. This mismatch (OQ-83 R5 rule) flags mandatrophy: the constraint is a zombie—the founding problem's contestability suggests the constraint was motivated more by ideology than necessity, and the world_rearranges verdict proves the constraint's persistence depends on active suppression of alternatives, not on solving an unavoidable coordination problem. If the founding problem is contested (Ottoman tradition could have been compatible with modern literacy), and if alternatives would re-emerge if enforcement stopped (world rearranges), then the constraint is extractive masquerading as coordination. The classification should flag mandatrophy: has_mandatrophy_declaration = true, and the mandate (rupture with Ottoman past) is obsolete if the founding problem is contestable. The constraint persists as pure extraction wearing the costume of 'modernization,' not because the modernization narrative is true but because beneficiaries profit from the suppression it justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_kernel_contest,
    'Is Latin script adoption a necessary structural break from Ottoman-Islamic modernity, or a contingent ideological choice that could have been made differently (gradual transition, bilingualism, phonetic improvement within Arabic script)?',
    'Comparative analysis of other post-imperial transitions (Egypt, Iran, Indonesia) that adopted different scripts or pluralistic approaches; counterfactual analysis of Turkish institutional development under alternative script regimes; historical linguistics showing whether Ottoman intellectuals could have reformed Arabic script phonetically for Turkish.',
    'If the rupture was contingent, the constraint is revealed as pure extraction riding on a cover story of ''necessary modernization''—reclassification from tangled_rope toward snare. If structurally necessary, the constraint''s coordination function is real and the high suppression is the price of irreversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_kernel_contest, conceptual, 'Whether script rupture is necessary or contingent to modernization.').

omega_variable(
    coordination_vs_authority_transfer,
    'Does the constraint solve a genuine coordination problem (multi-script literacy fragmentation), or is the coordination problem a post-hoc justification for state monopoly over meaning-making and cultural authority displacement?',
    'Examine pre-transition literacy levels, educational reach, and mutual intelligibility problems in Ottoman empire; measure whether literacy *increased* because of script coordination or whether the same literacy expansion could have occurred through other means (expansion of madrasas, European education, printed materials in existing scripts). Analyze whether suppression targets actual barriers to coordination or legitimate alternative authority structures.',
    'If coordination is real, the high suppression reflects enforcement of a genuine collective good against free-riders. If coordination is post-hoc rationalization, suppression is pure coercion, and the constraint is snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_authority_transfer, empirical, 'Whether the constraint solves a real coordination problem or merely covers extraction with a coordination narrative.').

omega_variable(
    reading_foreclosure_mechanism,
    'Does the Kemalist rupture reading logically foreclose the ottoman_continuity_reading (two cannot coexist in the same framework), or do both readings remain live options for different parties holding different commitments?',
    'Examine whether a single institutional framework could hold both readings: could an Ottoman-continuity advocate argue for literacy in Arabic script while accepting Turkish national identity? Could a Kemalist argue for phonetic improvement while honoring Ottoman intellectual tradition? If both are logically possible within distinct frameworks, the reading relationship is coexists_with, not forecloses.',
    'If the readings foreclose each other, the Kemalist reading''s core premise (rupture is necessary) directly contradicts continuity''s core premise (tradition is compatible with modernity). If they coexist, both are live readings of a contested kernel, and the constraint''s suppression exists precisely to deny the ottoman_continuity_reading legitimacy—constraint''s function is then political foreclosure, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Logical structure of the kernel contest: do sibling readings foreclose or coexist?').

omega_variable(
    suppression_structure_vs_internalization,
    'Is the measured suppression (0.71) primarily structural (state coercion, legal prohibition, institutional exclusion of Arabic script) or internalized (new generation internalizes the narrative that Latin script is modern and progress, that Arabic script is backward)?',
    'Generational analysis: measure resistance to the constraint among those who learned both scripts (can choose) versus those who only learned Latin script (suppression is invisible to them). Track private use of Arabic script, underground madrasas, exile of scholars—if substantial, suppression is structural; if minimal, suppression has become internalized norm. Examine whether suppression *requirements* decline over time as acceptance becomes culturally internalized.',
    'If structural, the constraint''s persistence depends on active enforcement infrastructure (education monopoly, printing controls, media regulation)—removal would expose the constraint immediately. If internalized, the constraint is self-perpetuating even if state coercion relaxes—targets carry suppression with them; exit becomes identity death, not just institutional barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structure_vs_internalization, empirical, 'Suppression mechanism: structural coercion, internalized norm, or both?').

omega_variable(
    identity_lock_arabic_script_literati,
    'Why do arabic_script_literati exit option classify as identity_locked rather than trapped? Is the professional identity truly inseparable from Arabic script mastery, or is it a choice to maintain identity rather than a structural inability to exit?',
    'Study cases of bilingual scholars who mastered both scripts and maintained intellectual authority—did they lose identity? Examine scholars who emigrated or went underground—did they abandon professional identity or maintain it in exile? If identity can survive script transition (with retraining), the exit is constrained rather than identity_locked, and suppression effectiveness depends on state''s ability to prevent retraining.',
    'If truly identity_locked, the constraint creates permanent elimination of a professional class—high human cost. If constrained (retraining available but suppressed), the constraint is brittle to regime change or to allowing parallel educational systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_arabic_script_literati, conceptual, 'Is script literacy truly identity or role-contingent skill?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(scri_tr_t0, projected).
narrative_ontology:measurement(scri_tr_t5, script_as_identity__kemalist_rupture_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(scri_tr_t5, observed).
narrative_ontology:measurement(scri_tr_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(scri_tr_t10, observed).
narrative_ontology:measurement(scri_tr_t15, script_as_identity__kemalist_rupture_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(scri_tr_t15, observed).
narrative_ontology:measurement(scri_tr_t20, script_as_identity__kemalist_rupture_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(scri_tr_t20, observed).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__kemalist_rupture_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(scri_tr_t30, observed).
narrative_ontology:measurement(scri_tr_t40, script_as_identity__kemalist_rupture_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(scri_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(scri_be_t0, projected).
narrative_ontology:measurement(scri_be_t5, script_as_identity__kemalist_rupture_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(scri_be_t5, observed).
narrative_ontology:measurement(scri_be_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(scri_be_t10, observed).
narrative_ontology:measurement(scri_be_t15, script_as_identity__kemalist_rupture_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(scri_be_t15, observed).
narrative_ontology:measurement(scri_be_t20, script_as_identity__kemalist_rupture_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(scri_be_t20, observed).
narrative_ontology:measurement(scri_be_t30, script_as_identity__kemalist_rupture_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(scri_be_t30, observed).
narrative_ontology:measurement(scri_be_t40, script_as_identity__kemalist_rupture_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(scri_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(scri_su_t0, projected).
narrative_ontology:measurement(scri_su_t5, script_as_identity__kemalist_rupture_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(scri_su_t5, observed).
narrative_ontology:measurement(scri_su_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(scri_su_t10, observed).
narrative_ontology:measurement(scri_su_t15, script_as_identity__kemalist_rupture_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(scri_su_t15, observed).
narrative_ontology:measurement(scri_su_t20, script_as_identity__kemalist_rupture_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(scri_su_t20, observed).
narrative_ontology:measurement(scri_su_t30, script_as_identity__kemalist_rupture_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(scri_su_t30, observed).
narrative_ontology:measurement(scri_su_t40, script_as_identity__kemalist_rupture_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(scri_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% The constraint 'script as identity' is a contested kernel with three structurally distinct instantiations. The Kemalist rupture reading (this story) frames Latin script adoption as necessary for secular modernity and breaks with Ottoman tradition. The ottoman_continuity_reading frames it as severing a coherent intellectual tradition. The phonetic_instrumentalism_reading frames it as a neutral technical choice. These are not different measurements of one constraint—their ε values differ substantively (rupture reading: extractive, high suppression; continuity reading: epistemic loss, low technical extraction; phonetic reading: minimal extraction, pure coordination). Each reading has its own stakeholder structure, beneficiaries/victims, and type classification. This story links to its siblings via network.affects_constraints to enable contamination analysis: if one reading's purity degrades (beneficiaries captured, extraction revealed), the system can predict how the rival readings become more salient (coexists_with relationship). The trunk constraint script_as_identity is not authored as a separate story—it is the kernel itself, contested among the three readings. See constraint_story_schema.json: ε-invariance principle (DP-001) requires separate stories for structurally distinct observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
