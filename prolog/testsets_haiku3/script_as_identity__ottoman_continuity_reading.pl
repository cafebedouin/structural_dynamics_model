% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Turkish-Islamic Identity and Ottoman Institutional Continuity
 *   domain: linguistic/political/religious
 *
 * SUMMARY:
 *   This constraint story instantiates the OTTOMAN CONTINUITY READING of the
 *   contested kernel: 'script as identity.' Under this reading, Arabic script
 *   is constitutive of Turkish-Islamic identity and the institutional
 *   continuity of Ottoman-derived state authority. The reading grounds script
 *   preservation in Islamic scholarly authority, Ottoman administrative
 *   legitimacy, and the epistemological gatekeeping function of the religious
 *   scholarly class. This is ONE reading of a three-way kernel contest: the
 *   KEMALIST RUPTURE READING holds that Latin script enables secular
 *   modernization by severing Ottoman-Islamic past; the PHONETIC
 *   INSTRUMENTALISM READING treats script as neutral technology where Latin
 *   provides superior phonetic transparency. This story describes the
 *   standing arrangement under contest (Arabic script maintained within state
 *   and religious institutions) as the ottoman_continuity_reading sees it:
 *   extraction high because it suppresses mass literacy and reserves
 *   epistemic authority; beneficiaries are religious scholars and
 *   institutional continuity constituencies; victims are non-literate
 *   populations and secular modernizers. The reading's authorization base
 *   runs through Islamic scholarly tradition and Ottoman institutional
 *   lineage, not through rational-choice or phonetic-technical grounds — that
 *   doctrinal grounding is stored in cs_structure.authority_grounding and
 *   cs_structure.axioms.
 *
 * KEY AGENTS:
 *   - Religious scholarly class (ulema, jurists): institutional beneficiary; authority structure depends on Arabic gatekeeping
 *   - Ottoman institutional continuity constituencies (state institutions, courts): institutional beneficiary; legitimacy chain preserved through script continuity
 *   - Turkish-speaking general population: powerless payer; bears literacy suppression cost and dependence on intermediaries
 *   - Secular modernizing coalition (nationalist technocrats, intellectuals): powerful payer; constrained from implementing script change by religious establishment strength
 *   - Ottoman archive custodians: analytical observer; face technical preservation questions but retain choice of infrastructure approach
 *   - Islamic religious authority structure (doctrine, non-agent): institutional beneficiary; represented operationally by scholars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.81).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Turkish-Islamic Identity and Ottoman Institutional Continuity").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "linguistic/political/religious").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, '25d5ba9e-7355-4bcd-a555-9cede6e253cd').
narrative_ontology:cs_kernel_codification('25d5ba9e-7355-4bcd-a555-9cede6e253cd', fixed_text).
narrative_ontology:cs_authority_grounding('25d5ba9e-7355-4bcd-a555-9cede6e253cd', lineage).
narrative_ontology:cs_interpretation_layer_present('25d5ba9e-7355-4bcd-a555-9cede6e253cd').
narrative_ontology:cs_reading_relation('25d5ba9e-7355-4bcd-a555-9cede6e253cd', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('25d5ba9e-7355-4bcd-a555-9cede6e253cd', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('25d5ba9e-7355-4bcd-a555-9cede6e253cd', foundational, arabic_script_epistemological_necessity).
narrative_ontology:cs_axiom_status(arabic_script_epistemological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('25d5ba9e-7355-4bcd-a555-9cede6e253cd', arabic_script_epistemological_necessity, deontological).
narrative_ontology:cs_axiom('25d5ba9e-7355-4bcd-a555-9cede6e253cd', foundational, ottoman_institutional_lineage_legitimacy).
narrative_ontology:cs_axiom_status(ottoman_institutional_lineage_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25d5ba9e-7355-4bcd-a555-9cede6e253cd', ottoman_institutional_lineage_legitimacy, conventional).
narrative_ontology:cs_reference_frame('25d5ba9e-7355-4bcd-a555-9cede6e253cd', ottoman_islamic_scholarly_continuity).
narrative_ontology:cs_drift_state('25d5ba9e-7355-4bcd-a555-9cede6e253cd', kemalist_state_consolidation_1923_1945, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25d5ba9e-7355-4bcd-a555-9cede6e253cd', '2026-06-19T14:32:18Z').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, religious_scholarly_class).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_institutional_continuity_constituencies).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_arabic_literate_populations).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, secular_modernizing_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, turkish_speaking_general_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ulema, Islamic jurists, and religious scholars whose authority, training, and epistemic legitimacy depend on Arabic textual mastery. Arabic script preservation maintains their gatekeeping over legal and theological interpretation, preserves access to centuries of Ottoman fatwa literature and Islamic jurisprudence, and positions them as irreplaceable intermediaries between state law and Islamic authority. Their professional identity is constituted through Arabic competence.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, religious_scholarly_class, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, religious_scholarly_class, agenda_setter).

% State institutions, administrative courts, military establishments, and legal authorities whose legitimacy chain runs backward through Ottoman precedent. Arabic script preserves readability of Ottoman archives, judicial records, and administrative continuity. Maintaining script continuity allows institutional actors to claim unbroken authority descent from Ottoman governance rather than rupture and restart. Institutional identity fuses with script continuity.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_institutional_continuity_constituencies, beneficiary,
    institutional, generational, constrained, national).

% Bear the suppression cost: reduced literacy rates when Arabic script does not map transparently to Turkish phonetics; limited access to new technical, scientific, and administrative literacy; dependence on specialized translators and intermediaries; cognitive load of learning a non-phonetic writing system. Cannot opt out of reading and writing their own language.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, turkish_speaking_general_population, payer,
    powerless, biographical, trapped, national).

% Nationalist intellectuals, technocrats, and state-building elites who view Arabic script as a barrier to mass literacy, technical modernization, and secular nation-state identity. They argue phonetic transparency (Latin alphabet) and script change enable rapid education and integration into international scientific and commercial order. Their institutional dominance is constrained by religious establishments' grip on the script question.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, secular_modernizing_coalition, payer,
    powerful, biographical, constrained, national).

% Historians, archivists, and documentary institutions that face the technical challenge of preserving and transmitting Ottoman-era written records. Arabic script preserves direct continuity of historical documents; script change creates a historical rupture requiring specialized transliteration and translation infrastructure. They can choose to develop that infrastructure or maintain Arabic literacy for archival work.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_archive_custodians, observer,
    institutional, generational, analytical, national).

% Non-agent placeholder: the doctrine and institutional framework that derives authority from Qur'anic Arabic and the Islamic scholarly tradition. Represented operationally by the religious scholarly class, but kept separate to avoid conflating the doctrine (a non-actor claim) with the agents who benefit from maintaining it.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, islamic_religious_authority_structure, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(script_as_identity__ottoman_continuity_reading, islamic_religious_authority_structure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, religious_scholarly_class).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single epistemological and institutional frame connecting contemporary Turkish state and society to Ottoman-Islamic legal precedent and Islamic scholarly authority. Solves the governance problem of how to ground contemporary state legitimacy in historical continuity while preserving Islamic religious authority structures that mediate law and social order.
% TRANSFER_FUNCTION: Transfers the cost of script mastery and literacy access from educated religious and institutional elites to the general population. Moves epistemic authority from technical/secular knowledge holders to Arabic-literate religious scholars. Moves institutional legitimacy from rupture narratives (nation-state as new start) to continuity narratives (state as Ottoman heir). The extraction flows as reduced literacy opportunity and dependence on scholarly intermediaries.
% ABSENT_VOICES: Excluded: the general Turkish-speaking population who bear literacy costs but have minimal voice in script policy (they are trapped, not absent—they cannot opt out and cannot organize effectively around the constraint); non-Arab Islamic peoples in Ottoman-successor regions who might argue for vernacular script but are geographically/politically outside the Turkish nation-state frame.
% DISAPPEARANCE_RATIONALE: If this constraint dissolved (Arabic script abandoned for Latin), Turkish religious scholars would lose their gatekeeping function as sole readers of Ottoman legal and religious texts; state legitimacy would reframe from Ottoman continuity to national rupture or international modernization; Islamic institutional authority would either adapt or fragment. The reading and writing practices of the entire Turkish-speaking population would shift; Ottoman archives would require systematic transliteration infrastructure or would become accessible only to specialists; religious instruction and scholarly training would separate from Arabic textual mastery; the symbolic link between Turkish identity and Islamic historical continuity would require active ideological work rather than being encoded in daily literacy practice.
% FOUNDING_PROBLEM: How to maintain Turkish state legitimacy as continuous with Ottoman Islamic governance after Ottoman political collapse and military defeat, while preserving Islamic religious authority within a modern centralized nation-state structure. Script continuity serves this by maintaining institutional and epistemological connection to Ottoman precedent, making contemporary state authority appear as natural heir to Ottoman institutions rather than as revolutionary rupture.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutional actors and conservative nationalist historians (Ziya Gökalp, early Atatürk-era Islamic preservationists) attest the founding problem is live and ongoing—Turkish state identity cannot be severed from Ottoman-Islamic heritage without losing institutional legitimacy. Secular modernizing intellectuals and Kemalist reformers (Mustafa Kemal, Halide Edib, language reformers) attest the founding problem is constructed—Turkish state legitimacy can be effectively refrounded on secular nationalism and international modernization without Ottoman institutional continuity; script change is part of that refounding, not a threat to legitimacy. Independent Ottoman historians and Islamic law scholars describe historical precedents for script transitions that preserved institutional continuity (Mamluk administrative script shifts, Persian-Turkish literary transitions) and cases where script change did not destabilize religious authority (Indonesia, Pakistan after script standardization). This corroboration suggests the founding problem is ambiguous rather than clearly live or dead.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint preserves the religious scholarly class's epistemic gatekeeping and institutional legitimacy while imposing literacy costs on the general population—the transfer is asymmetric and decoupled from service provision (religious scholars do not provide literacy instruction in exchange for gating authority; they gate authority as a structural fact). Suppression (0.81) is higher still because the constraint persists by actively defending against Latin script adoption despite the secular coalition's institutional power and despite technical arguments for phonetic transparency. The defense machinery includes religious authority claims, institutional legitimacy narratives, and cultural-identity framing that makes script change linguistically/culturally unthinkable for many constituencies. Theater (0.42) is moderate: genuine coordination value exists (Ottoman continuity is real institutional scaffolding), but growing share of suppressive effort defends the gatekeeping function rather than the coordination problem itself. Accessibility collapse (0.72) is high because alternatives (Latin script adoption) are technically available but politically/religiously foreclosed—the constraint's persistence depends not on physical impossibility but on doctrinal and institutional closure. Resistance (0.59) is moderate because the secular coalition is institutionally powerful but not dominant; they can propose Latin script but cannot overcome religious establishment's structural grip without political rupture. Measurement series spans 1923–1945 (early Turkish Republic period): extractiveness and suppression both rise over this interval as the state consolidates and religious authority becomes explicitly defensive rather than taken-for-granted; theater rises as the defensive work becomes performative (emphasizing identity and continuity rather than actual service provision).
 *
 * PERSPECTIVAL GAP:
 *   This is the core measurement the story exists to surface: the religious scholarly class and institutional continuity constituencies experience the SAME constraint as coordination that preserves essential epistemic and institutional structures; the general population and secular coalition experience it as suppression that reserves authority and literacy access for political elites; these divergent readings are not measurement error, they are structural properties of asymmetric directionality. The engine computes each seat's type from the structural data; where types diverge, that is the pattern the corpus exists to detect. A story whose claim and metrics 'agree' may be a well-integrated constraint, but a story whose claim diverges from the computed types is often the more interesting case—it reveals either that the claiming seat is mischaracterizing the arrangement or that the arrangement serves genuinely different functions at different seats. This story claims tangled_rope (coordination + asymmetric extraction) and the metrics are authored to describe that mixed function; the beneficiary seats compute rope or coordination (high d toward beneficiary end), the payer seats compute snare or tangled_rope (high d toward target end). That divergence is the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars occupy the beneficiary/agenda-setter seat: they set the cultural and institutional conditions for script preservation, benefit from preserved gatekeeping, and have identity_locked relationship to Arabic (their professional identity is constituted through Arabic mastery). From their seat, the arrangement is genuine coordination—it preserves institutional legitimacy and scholarly authority structures that they see as socially necessary. Ottoman institutional constituencies (courts, state bureaucracies) occupy a similar beneficiary seat: they control script policy but depend on continuity narratives for their own legitimacy. The secular coalition occupies a constrained-payer seat: they have institutional power to propose alternatives but face religious establishment veto; their exit is blocked by political coalition dynamics and cultural-identity frames that make script change existentially threatening to the nation-state's perceived link to Islamic past. The general population occupies a trapped-payer seat: they cannot exit literacy (it is compulsory), cannot exit Turkish (it is their language), and have minimal voice in script policy. From the general population's seat, the arrangement is pure extraction—they bear the suppression cost of non-phonetic script with no compensation or coordination benefit. The engine derives these directionalities from the structural data (beneficiary/victim declarations + power + exit options); the spread in directionality values generates seat-level type divergence: the beneficiary seat computes tangled_rope or rope; the payer seats compute snare or tangled_rope depending on their resistance capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain Turkish state legitimacy as continuous with Ottoman Islamic governance after Ottoman collapse) was live at the 1923 constraint origin. By 1945, secular institutional consolidation and mass literacy pressure create a contestable question: is the founding problem still live, or has it been superseded by different legitimacy questions (national modernization, international integration)? The ottoman_continuity_reading maintains that the founding problem is permanently live because Turkish state legitimacy inherently depends on institutional connection to Ottoman precedent; the kemalist_rupture_reading holds that the problem has been solved by constituting a new republican nationalism independent of Ottoman legitimacy. The rising theater_ratio (from 0.18 to 0.42 over 1923–1945) suggests that the suppressive work of maintaining script continuity is increasingly theatrical—the functional justification weakens while the ideological justification strengthens. This is a mandatrophy candidate: if the founding problem has genuinely been superseded, the constraint persists purely through institutional inertia and identity narratives, not through actual coordination necessity. The R5 mismatch to monitor: founding_problem_status=contested + disappearance_verdict=world_rearranges + rising theater_ratio would suggest the constraint is transitioning from tangled_rope (mixed coordination/extraction) to piton (pure inertia defended narratively). The omegas (below) document the irreducible uncertainty about whether the founding problem is live or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_or_dead,
    'Is the founding problem (maintaining Turkish state legitimacy as continuous with Ottoman-Islamic past) still genuinely live, or has it been functionally superseded by nationalist legitimacy narratives that no longer depend on Ottoman institutional continuity?',
    'Historical counterfactual: if Latin script were adopted and Ottoman archive access required specialized translation infrastructure, would Turkish state legitimacy collapse or merely require reframing? Post-1945 institutional history: does Turkish state authority require ongoing Ottoman script continuity or does republican nationalism provide sufficient legitimacy base?',
    'If the problem is dead, the constraint''s persistence is mandatrophy—institutional inertia + identity narrative, not functional coordination. Rising theater_ratio suggests the problem may be dead. If the problem is live, the constraint is genuine tangled_rope (coordination + extraction). This determines whether remedies should target script change (if dead) or reform within Arabic-script constraint (if live).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, conceptual, 'Whether Ottoman institutional continuity is a founding necessity or a historical legacy available for superseding.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression (0.81) is structural (Arabic script''s phonetic mismatch with Turkish) versus internalized (Turkish speakers'' cultural learning that script change is national betrayal)?',
    'Comparative history: script transitions in Arabic-writing Islamic societies (Ottoman-to-Latin shifts in Turkey, Egypt''s Arabic-Latin debates, Iran''s Persian-script preservation). Post-script-change learning curves: do Turkish speakers who learn Latin script show rapid literacy improvement, suggesting suppression was structural + learnable? Psychological studies of script-identity fusion.',
    'If suppression is mostly structural, phonetic reforms or Latin transition could reduce extraction substantially. If suppression is mostly internalized, script change alone would not free the constraint—identity-fusion barriers would persist in new symbolic form. Internalized suppression suggests the constraint''s grip is stronger and longer-lasting than technical solutions can address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of literacy suppression: orthographic vs. cultural-identity-based.').

omega_variable(
    kernel_reading_alternative_framings,
    'Could the constraint described here be coherently instantiated under the KEMALIST RUPTURE READING (Latin script enables secular modernization by severing Ottoman-Islamic past) or the PHONETIC INSTRUMENTALISM READING (script is neutral technology; Latin provides superior phonetic transparency)?',
    'Reframe the same standing arrangement (Turkish script policy during 1923–1945) under each alternative reading''s premises. If the alternative reading cannot coherently describe this history without internal contradiction, the readings foreclose each other. If the alternative reading can describe the same history with different causal narratives and different victim/beneficiary assignments, the readings coexist.',
    'If readings foreclose (one reading''s axioms directly contradict another''s), then script-choice is not genuinely contested—one reading wins and others become incoherent. If readings coexist, then the kernel-level contest is genuinely open and multiple policy directions are defensible from different reading positions. This affects whether constraint remedies require framework-level resolution (foreclosure case) or local negotiation within coexisting frameworks (coexistence case).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether the sibling readings of the script-identity kernel foreclose each other or coexist as live alternatives.').

omega_variable(
    islamic_authority_necessity_claim,
    'Is Islamic religious authority genuinely DEPENDENT on Arabic script gatekeeping, or could Islamic institutional authority persist and adapt under a script transition?',
    'Historical precedent: Islamic scholarship survived major script transitions (Mamluk-to-Ottoman shifts in administrative script, Arabic-to-Persian-to-Turkish in literary tradition). Contemporary comparisons: Muslim-majority nations using non-Arabic scripts (Indonesia, Pakistan, Turkey post-1928) and the fate of Islamic institutional authority in those cases. Theological analysis: do Islamic doctrines logically require Arabic script or only Arabic language (readably in any script)?',
    'If Islamic authority is genuinely script-dependent, removing Arabic script risks institutional collapse of religious scholarly gatekeeping—extraction protection is structurally necessary. If Islamic authority could adapt to script transition, then suppression of script change is a choice rather than necessity—the constraint becomes more clearly extractive. This determines whether the constraint''s persistence is defending an essential function or defending an accidental historical coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(islamic_authority_necessity_claim, empirical, 'Whether Islamic institutional authority logically depends on Arabic script or only happens to be historically coupled to it.').

omega_variable(
    identity_locked_exit_reversibility,
    'For Turkish religious scholars with identity_locked exit to Arabic script, how reversible is the identity-fusion if script change were externally imposed? Could a scholar''s professional identity reorient to Latin-script Islamic scholarship, or does the identity lock irreversibly bind them to Arabic?',
    'Post-script-change outcomes: in countries that transitioned scripts, do religious scholars who learned the new script show career continuity and professional identity persistence, or do they experience career rupture and identity crisis? Psychological and sociological studies of identity-fusion breakpoints in professional communities.',
    'If identity lock is reversible (scholars could adapt), the extraction cost of suppression is higher than the necessary cost of functional continuity—the constraint is more clearly extractive. If identity lock is irreversible, the constraint''s persistence depends on defending scholars'' structural position against obsolescence—extraction is entangled with genuine institutional preservation necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_reversibility, empirical, 'Whether identity-locked Turkish scholars could adapt to script change or would experience permanent professional displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1923, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1923, script_as_identity__ottoman_continuity_reading, theater_ratio, 1923, 0.18).
narrative_ontology:measurement(scri_tr_t1926, script_as_identity__ottoman_continuity_reading, theater_ratio, 1926, 0.24).
narrative_ontology:measurement(scri_tr_t1929, script_as_identity__ottoman_continuity_reading, theater_ratio, 1929, 0.32).
narrative_ontology:measurement(scri_tr_t1932, script_as_identity__ottoman_continuity_reading, theater_ratio, 1932, 0.38).
narrative_ontology:measurement(scri_tr_t1938, script_as_identity__ottoman_continuity_reading, theater_ratio, 1938, 0.42).
narrative_ontology:measurement(scri_tr_t1945, script_as_identity__ottoman_continuity_reading, theater_ratio, 1945, 0.42).

% Extraction over time
narrative_ontology:measurement(scri_be_t1923, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1923, 0.52).
narrative_ontology:measurement(scri_be_t1926, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1926, 0.58).
narrative_ontology:measurement(scri_be_t1929, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1929, 0.63).
narrative_ontology:measurement(scri_be_t1932, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1932, 0.65).
narrative_ontology:measurement(scri_be_t1938, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1938, 0.68).
narrative_ontology:measurement(scri_be_t1945, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1945, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1923, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1923, 0.64).
narrative_ontology:measurement(scri_su_t1926, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1926, 0.71).
narrative_ontology:measurement(scri_su_t1929, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1929, 0.76).
narrative_ontology:measurement(scri_su_t1932, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1932, 0.79).
narrative_ontology:measurement(scri_su_t1938, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1938, 0.81).
narrative_ontology:measurement(scri_su_t1945, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1945, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, ottoman_elite_literacy_gatekeeping).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, religious_institutional_authority_structure__turkey).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel script_as_identity. The sibling reading kemalist_rupture_reading describes the same historical standing arrangement (Turkish script policy 1923–1945) but frames it as pure suppression of modernization rather than coordination of institutional continuity. The phonetic_instrumentalism_reading treats script as neutral technology and locates the constraint's function in phonetic transparency rather than identity/authority preservation. Each reading has different ε, different victim/beneficiary structure, different type classification. They are not observational variations on one constraint—they are three distinct constraints grounded in three distinct authority framings of one contested kernel. The network edges record which constraints are upstream of which (this reading affects the kemalist reading's legitimacy claims; phonetic reading influences but does not foreclose this reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
